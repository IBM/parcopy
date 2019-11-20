{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module MultiRm where

import           Control.Lens        ((<%~), _init, at, forOf_, non, view)
import           Control.Monad       (when)
import qualified Data.Map.Strict     as Map
import           Data.Sequence       (Seq(..))
import qualified Data.Sequence       as Seq
import           Data.Tuple          (swap)
import           Lib
import           Options.Applicative
import           System.FilePath     ((</>))
import           UnliftIO.Directory  (listDirectory)
import           UnliftIO.IORef      (newIORef, atomicModifyIORef')

multiRm :: FilePath -> Work ()
multiRm target = do
    mountPoint <- view mountPoint
    entryCountsRef <- newIORef Map.empty

    multi [Seq.empty] $ \pathPcs workerId -> do
        let toPath pcs =
                mountPoint </> show workerId </> target </> foldr (</>) "" pcs
            path = toPath pathPcs

            -- Delete an item along with any newly empty dirs up the tree.
            go exe pcs = do
                run exe ["-v", toPath pcs]

                forOf_ _init pcs $ \parentPcs -> do
                    -- Decrement parent, removing it if it's zero, and report
                    -- the new count.
                    newCount <- atomicModifyIORef' entryCountsRef $
                        swap . (at parentPcs . non 0 <%~ subtract 1)

                    when (newCount == 0) $ go "rmdir" parentPcs

        tryFileTypes [
            listDirectory path >>= \case
                -- Empty dir.  Delete it.
                [] -> do
                    go "rmdir" pathPcs
                    return []

                -- Dir with entries.  Keep track of how many it has so it can be
                -- known when to delete it, and add them to the work store.
                entries -> do
                    atomicModifyIORef' entryCountsRef $
                        (, ()) . Map.insert pathPcs (length entries)
                    return $ map (pathPcs :|>) entries,

            do
                -- File.  Delete it.
                go "rm" pathPcs
                return []]

main :: IO ()
main = mkUtil "Recursive delete in parallel" $ multiRm <$> target
    where
    target = strArgument $ mconcat [
        metavar "TARGET",
        help "Directory to remove"]
