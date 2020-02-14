{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib (
    LogLevel(..),
    Work,
    log,
    multi,
    mkUtil,
    run,
    tryFileTypes)
where

import Control.Concurrent
import Control.Exception.Lens       (_IOException, catching_)
import Control.Lens
import Control.Monad.IO.Unlift      (withRunInIO)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.List                    (intercalate)
import Options.Applicative
import Prelude                      hiding (log)
import System.FilePath              ((</>))
import System.IO.Error.Lens         (_InappropriateType, errorType)
import System.Posix.Files           (setOwnerAndGroup)
import System.Posix.Process         (forkProcess, getProcessStatus)
import System.Posix.Temp            (mkdtemp)
import System.Posix.Types           (GroupID, UserID)
import System.Posix.User
import UnliftIO.Async               (forConcurrently_)
import UnliftIO.Directory           (makeAbsolute)
import UnliftIO.Environment         (getProgName)
import UnliftIO.Process             (readProcess)
import UnliftIO.STM

-----------
-- Types --
-----------

data LogLevel
    = Quiet
    | Verbose
    | Debug
    deriving (Eq, Ord)

type Logger = LogLevel -> String -> IO ()

data WorkContext
    = WorkContext {
        workContextLogger :: Logger,
        workContextMounts :: [FilePath]}
makeFields ''WorkContext

type Work = ReaderT WorkContext IO

data Config
    = Config {
        configLogLevel   :: LogLevel,
        configNumThreads :: Int,
        configUncName    :: String,
        configCredFile   :: FilePath,
        configWork       :: Work ()}
makeFields ''Config

data SetupContext
    = SetupContext {
        setupContextLogger         :: Logger,
        setupContextOrigUidGidNcap :: (UserID, GroupID, Int),
        setupContextNumThreads     :: Int,
        setupContextUncName        :: String,
        setupContextCredFile       :: FilePath}
makeFields ''SetupContext

type Setup = ReaderT SetupContext (ResourceT IO)

-----------------------
-- General utilities --
-----------------------

chompNewlines :: String -> String
chompNewlines s = case span (== '\n') s of
    (newlines, c : cs) -> newlines ++ c : chompNewlines cs
    _                  -> ""

log :: (HasLogger r Logger, MonadIO m, MonadReader r m) =>
    LogLevel -> String -> m ()
log lvl msg = do
    logger <- view logger
    liftIO $ logger lvl msg

run :: (HasLogger r Logger, MonadIO m, MonadReader r m) =>
    LogLevel -> FilePath -> [String] -> m ()
run lvl exe args = do
    log Debug $ unwords $ "Starting" : exe : args
    readProcess exe args "" >>= log lvl

fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 x f = fix f x

-- Like allocate, but actions have access to an environment.
allocate' :: (MonadResource m, MonadReader r m) =>
    (ReaderT r IO a) -> (a -> ReaderT r IO ()) -> m (ReleaseKey, a)
allocate' now later = do
    ctx <- ask
    allocate (runReaderT now ctx) (\x -> runReaderT (later x) ctx)

-----------
-- Setup --
-----------

-- We need to be real root for mount(8) and umount(8).  In between, we should do
-- file ops as normal user.  The solution for now is forkProcess; see
-- https://github.com/haskell/unix/issues/62 for caveats.
setupParent :: Setup (Work ())
setupParent = do
    changeUid 0
    changeGid 0
    changeNumCap 1

    -- Action to restore all of the above in the child.
    views origUidGidNcap $ \(uid, gid, ncap) -> do
        changeNumCap ncap
        changeGid gid
        changeUid uid

    where

    changeUid uid = do
        log Debug $ "Setting UID to " ++ show uid ++ "..."
        liftIO $ setUserID uid
        log Debug $ "Set UID to " ++ show uid
    changeGid gid = do
        log Debug $ "Setting GID to " ++ show gid ++ "..."
        liftIO $ setGroupID gid
        log Debug $ "Set GID to " ++ show gid
    changeNumCap numCap = do
        log Debug $ "Setting number of capabilities to " ++ show numCap ++ "..."
        liftIO $ setNumCapabilities numCap
        log Debug $ "Set number of capabilities to " ++ show numCap

setupMounts :: Setup [FilePath]
setupMounts = do
    (_, parentDir) <- allocate' createTempDir removeDir

    numThreads <- view numThreads
    let dirs = map (\i -> parentDir </> show i) [0 .. numThreads - 1]

    forM_ dirs $ \dir -> allocate' (createDir dir) (\_ -> removeDir dir)
    forConcurrently_ dirs $ \dir -> allocate' (mountCifs dir) (\_ -> umount dir)

    return dirs

    where

    createTempDir = do
        log Debug "Creating temporary directory..."
        progName <- getProgName
        dir <- liftIO $ mkdtemp $ "/tmp" </> progName ++ "-"
        log Debug $ "Created temporary directory " ++ dir
        chownOrig dir
        return dir
    createDir dir = do
        run Debug "mkdir" ["-v", dir]
        chownOrig dir
    chownOrig file = do
        (uid, gid, _) <- view origUidGidNcap
        let msg = file ++ " " ++ show uid ++ ":" ++ show gid
        log Debug $ "Chowning " ++ msg ++ "..."
        liftIO $ setOwnerAndGroup file uid gid
        log Debug $ "Chowned " ++ msg
    removeDir dir = run Debug "rmdir" ["-v", dir]
    mountCifs dir = do
        uncName <- view uncName
        credFile <- view credFile >>= makeAbsolute
        run Debug "mount" [
            "-v",
            "-t", "cifs",
            uncName,
            dir,
            "-o", intercalate "," [
                "vers=3.0",
                "credentials=" ++ credFile,
                "dir_mode=0777",
                "file_mode=0777",
                "serverino"]]
    umount dir = run Debug "umount" ["-v", "-l", dir]

-----------------
-- Application --
-----------------

multi :: [a] -> (a -> FilePath -> Work [a]) -> Work ()
multi initialWork processWork = do
    workStore <- newTVarIO initialWork
    numAtWorkSem <- newTVarIO (0 :: Int)

    mounts <- view mounts
    forConcurrently_ mounts $ \mount -> fix1 [] $ \go newWork -> do
        maybeItem <- atomically $ do
            oldWork <- readTVar workStore

            case newWork ++ oldWork of
                -- More work.  Take an item and signal busy.
                item : items -> do
                    writeTVar workStore items
                    modifyTVar' numAtWorkSem (+ 1)
                    return $ Just item

                -- No more work.  Unless all other workers are at rest, in which
                -- case there won't be any, wait for more.
                [] -> readTVar numAtWorkSem >>= \case
                    0 -> return Nothing
                    _ -> retrySTM

        -- If we took an item, process it, signal at rest, and go again.
        forMOf_ _Just maybeItem $ \item -> do
            newWork' <- processWork item mount
            atomically $ modifyTVar' numAtWorkSem $ subtract 1
            go newWork'

tryFileTypes :: [Work a] -> Work a
tryFileTypes = foldr1 $ catching_ $
    _IOException . errorType . _InappropriateType

mkUtil :: String -> Parser (Work ()) -> IO ()
mkUtil desc workParser = do
    config <- parseCmdLine desc workParser

    -- Quick and dirty threadsafe logger.
    logger <- do
        mutex <- newMVar ()
        return $ \lvl msg -> when (lvl <= config ^. logLevel) $
            withMVar mutex $ \_ -> putStrLn $ chompNewlines msg

    origUidGidNcap <- liftM3 (,,)
        getRealUserID
        getRealGroupID
        getNumCapabilities

    runResourceT $ do
        let setup = (,) <$> setupParent <*> setupMounts
        (setupChild, mounts) <- runReaderT setup $ SetupContext {
            setupContextLogger         = logger,
            setupContextOrigUidGidNcap = origUidGidNcap,
            setupContextNumThreads     = config ^. numThreads,
            setupContextUncName        = config ^. uncName,
            setupContextCredFile       = config ^. credFile}

        let forkedWork = forkAndWait $ setupChild >> config ^. work
        lift $ runReaderT forkedWork $ WorkContext {
            workContextLogger = logger,
            workContextMounts = mounts}

parseCmdLine :: String -> Parser (Work ()) -> IO Config
parseCmdLine desc workParser = execParser $ info (helper <*> params) $ mconcat [
    header desc,
    fullDesc]

    where

    params = do
        verbose <- flag Quiet Verbose $ mconcat [
            short 'v',
            long "verbose",
            help "Print every file operation"]
        debug <- flag Quiet Debug $ mconcat [
            long "debug",
            help "Print lots of debug info"]
        numThreads <- option auto $ mconcat [
            short 't',
            long "threads",
            metavar "INT",
            value 1,
            showDefault]
        uncName <- strOption $ mconcat [
            short 'n',
            long "share-name",
            metavar "//HOST/SHARE",
            help "The UNC name of the SMB share"]
        credFile <- strOption $ mconcat [
            short 'c',
            long "credentials-file",
            metavar "FILENAME"]
        work <- workParser

        pure $ Config {
            configLogLevel   = max verbose debug,
            configNumThreads = numThreads,
            configUncName    = uncName,
            configCredFile   = credFile,
            configWork       = work}

forkAndWait :: Work () -> Work ()
forkAndWait action = do
    log Debug "Parent: forking..."
    pid <- withRunInIO $ \runInIO -> forkProcess $ runInIO $ do
        log Debug "Child: forked"
        action

    maybeStatus <- liftIO $ getProcessStatus True True pid
    log Debug $ "Child's status was " ++ show maybeStatus
