{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Concurrent                (newMVar, withMVar)
import Control.Monad.IO.Unlift           (MonadUnliftIO)
import Control.Monad.Reader              (ReaderT(..), asks, forM_, liftIO, when)
import Data.Function                     (fix)
import Options.Applicative
import Prelude                           hiding (log)
import System.Directory.Internal.Prelude (IOErrorType(..), ioeGetErrorType)
import UnliftIO.Async                    (forConcurrently_)
import UnliftIO.Exception                (catch, throwIO)
import UnliftIO.Process                  (readProcess)
import UnliftIO.STM

data LogLevel
    = Quiet
    | Verbose
    deriving (Eq, Ord)

data WorkContext
    = WorkContext {
        workContextNumThreads :: Int,
        workContextLogger :: String -> IO (),
        workContextLogLevel :: LogLevel,
        workContextMountPoint :: FilePath}

type Work = ReaderT WorkContext IO

log :: LogLevel -> String -> Work ()
log lvl msg = do
    maxLvl <- asks workContextLogLevel
    when (lvl <= maxLvl) $ do
        logger <- asks workContextLogger
        liftIO $ logger msg

run :: FilePath -> [String] -> Work ()
run exe args = do
    log Verbose $ unwords $ "Starting" : exe : args
    readProcess exe args "" >>= log Verbose

multi :: [a] -> (a -> Int -> Work [a]) -> Work ()
multi initialWork processWork = do
    workStore <- newTVarIO initialWork
    numAtWorkSem <- newTVarIO (0 :: Int)

    t <- asks workContextNumThreads
    forConcurrently_ [0 .. t - 1] $ \workerId -> flip fix [] $ \go newWork -> do
        maybeItem <- atomically $ do
            oldWork <- readTVar workStore

            case newWork ++ oldWork of
                -- More work.  Take an item and signal busy.
                item : items -> do
                    writeTVar workStore items
                    modifyTVar' numAtWorkSem (+ 1)
                    return $ Just item

                -- No more work.  Unless all other workers are at rest, in which
                -- case there's not *gonna* be any, wait for more.
                [] -> readTVar numAtWorkSem >>= \case
                    0 -> return Nothing
                    _ -> retrySTM

        -- If we took an item, process it, signal at rest, and go again.
        forM_ maybeItem $ \item -> do
            newWork' <- processWork item workerId
            atomically $ modifyTVar' numAtWorkSem $ subtract 1
            go newWork'

tryFileTypes :: (Foldable t, MonadUnliftIO m) => t (m a) -> m a
tryFileTypes = foldr1 $ \action0 action1 -> action0 `catch` \e ->
    if ioeGetErrorType e == InappropriateType then
        action1
    else
        throwIO e

mkUtil :: String -> Parser (Work ()) -> IO ()
mkUtil desc workParser = do
    (ctxWithLogger, work) <- parseCmdLine

    -- Quick and dirty threadsafe logger.
    logger <- do
        mutex <- newMVar ()
        return $ \msg -> withMVar mutex $ \_ -> putStr msg

    runReaderT work $ ctxWithLogger logger

    where
    parseCmdLine = execParser $ info (helper <*> params) $ mconcat [
        header desc,
        fullDesc]

    params = do
        verbose <- switch $ mconcat [
            short 'v',
            long "verbose",
            help "Print every file operation"]
        threads <- option auto $ mconcat [
            short 't',
            long "threads",
            metavar "INT",
            value 1,
            showDefault]
        mountPoint <- strOption $ mconcat [
            short 'm',
            long "mountpoint",
            metavar "MOUNTPOINT",
            help "The containing directory of mountpoints named 0, 1, etc."]
        work <- workParser

        pure $ (, work) $ \logger -> WorkContext {
            workContextNumThreads = threads,
            workContextLogger = logger,
            workContextLogLevel = if verbose then Verbose else Quiet,
            workContextMountPoint = mountPoint}
