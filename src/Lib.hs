{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Concurrent     (newMVar, withMVar)
import Control.Exception.Lens (_IOException, catching_)
import Control.Lens           (_Just, forOf_, makeFields, view)
import Control.Monad.Reader   (ReaderT(..), liftIO, when)
import Data.Function          (fix)
import Options.Applicative
import Prelude                hiding (log)
import System.IO.Error.Lens   (_InappropriateType, errorType)
import UnliftIO.Async         (forConcurrently_)
import UnliftIO.Process       (readProcess)
import UnliftIO.STM

data LogLevel
    = Quiet
    | Verbose
    deriving (Eq, Ord)

data WorkContext
    = WorkContext {
        workContextNumThreads :: Int,
        workContextLogger     :: String -> IO (),
        workContextLogLevel   :: LogLevel,
        workContextMountPoint :: FilePath}
makeFields ''WorkContext

type Work = ReaderT WorkContext IO

log :: LogLevel -> String -> Work ()
log lvl msg = do
    maxLvl <- view logLevel
    when (lvl <= maxLvl) $ do
        logger <- view logger
        liftIO $ logger msg

run :: FilePath -> [String] -> Work ()
run exe args = do
    log Verbose $ unwords $ "Starting" : exe : args
    readProcess exe args "" >>= log Verbose

multi :: [a] -> (a -> Int -> Work [a]) -> Work ()
multi initialWork processWork = do
    workStore <- newTVarIO initialWork
    numAtWorkSem <- newTVarIO (0 :: Int)

    t <- view numThreads
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
        forOf_ _Just maybeItem $ \item -> do
            newWork' <- processWork item workerId
            atomically $ modifyTVar' numAtWorkSem $ subtract 1
            go newWork'

tryFileTypes :: (Foldable t) => t (Work a) -> Work a
tryFileTypes = foldr1 $ catching_ $
    _IOException . errorType . _InappropriateType

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
            workContextLogger     = logger,
            workContextLogLevel   = if verbose then Verbose else Quiet,
            workContextMountPoint = mountPoint}
