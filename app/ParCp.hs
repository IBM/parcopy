module ParCp where

import           Control.Lens        (view)
import           Data.Sequence       (Seq(..))
import qualified Data.Sequence       as Seq
import           Lib
import           Options.Applicative
import           System.FilePath     ((</>))
import           UnliftIO.Directory  (listDirectory)

parCp :: FilePath -> FilePath -> Work ()
parCp source dest = multi [Seq.empty] $ \pathPcs mount -> do
    let path = foldr (</>) "" pathPcs
        sourcePath = source </> path
        destPath = mount </> dest </> path

    tryFileTypes [
        do
            -- Directory.  Create it and add its entries to the work store.
            entries <- listDirectory sourcePath
            run Verbose "mkdir" ["-v", destPath]
            return $ map (pathPcs :|>) entries,

        do
            -- File.
            run Verbose "cp" ["-v", sourcePath, destPath]
            return []]

main :: IO ()
main = mkUtil "Recursive copy in parallel" $ parCp <$> source <*> dest

    where

    source = strArgument $ mconcat [
        metavar "SOURCE",
        help "Directory to copy contents from"]
    dest = strArgument $ mconcat [
        metavar "DESTINATION",
        help "Directory to copy contents to"]
