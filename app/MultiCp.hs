module MultiCp where

import           Control.Lens         (view)
import           Data.Sequence        (Seq(..))
import qualified Data.Sequence        as Seq
import           Lib
import           Options.Applicative
import           System.FilePath      ((</>))
import           UnliftIO.Directory   (listDirectory)

multiCp :: FilePath -> FilePath -> Work ()
multiCp source dest = do
    mountPoint <- view mountPoint

    multi [Seq.empty] $ \pathPcs workerId -> do
        let path = foldr (</>) "" pathPcs
            sourcePath = source </> path
            destPath = mountPoint </> show workerId </> dest </> path

        tryFileTypes [
            do
                -- Directory.  Create it and add its entries to the work store.
                entries <- listDirectory sourcePath
                run "mkdir" ["-v", destPath]
                return $ map (pathPcs :|>) entries,

            do
                -- File.  Copy it.
                run "cp" ["-v", sourcePath, destPath]
                return []]

main :: IO ()
main = mkUtil "Recursive copy in parallel" $ multiCp <$> source <*> dest
    where
    source = strArgument $ mconcat [
        metavar "SOURCE",
        help "Directory to copy contents from"]
    dest = strArgument $ mconcat [
        metavar "DESTINATION",
        help "Directory to copy contents to"]
