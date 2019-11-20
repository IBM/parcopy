{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module MultiRm where

import           Control.Monad.Reader (asks, forM_, when)
import           Data.Foldable        (toList)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Sequence        (Seq(..))
import qualified Data.Sequence        as Seq
import           Data.Tuple           (swap)
import           Lib
import           Options.Applicative
import           System.FilePath      (joinPath)
import           UnliftIO.Directory   (listDirectory)
import           UnliftIO.IORef       (newIORef, atomicModifyIORef')

-- Helper function.
preview_init :: Seq a -> Maybe (Seq a)
preview_init (init :|> _) = Just init
preview_init Empty        = Nothing

-- Helper function.  Decrement a count in a map, removing it if it's zero, and
-- report the new count.
decrAt :: (Eq k, Ord k) => k -> Map k Int -> (Map k Int, Int)
decrAt key = swap . Map.alterF f key
    where
    f (Just n) = let n' = n - 1 in (n', if n' == 0 then Nothing else Just n')

multiRm :: FilePath -> Work ()
multiRm target = do
    mountPoint <- asks workContextMountPoint
    entryCountsRef <- newIORef Map.empty

    multi [Seq.empty] $ \pathPcs workerId -> do
        let toPath pcs = joinPath $
                mountPoint : show workerId : target : toList pcs
            path = toPath pathPcs

            -- Delete an item along with any newly empty dirs up the tree.
            go exe pcs = do
                run exe ["-v", toPath pcs]

                forM_ (preview_init pcs) $ \parentPcs -> do
                    newCount <- atomicModifyIORef' entryCountsRef $
                        decrAt parentPcs

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
