module Files where

import System.Directory
import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False


isIgnored :: FilePath -> Bool
isIgnored name = Set.member name ignoredDirectories


-- Side effects

collectFiles :: FilePath -> IO [FilePath]
collectFiles p = (liftA2 (++)) filesM (concat <$> (mapM collectFiles =<< dirsM))
  where
    preds    = combinePreds [not . isIgnored, not . isHidden]
    filtered = (map ((appendOnce '/' p) <>) . filter preds) <$> listDirectory p
    split    = partitionM doesFileExist =<< filtered
    filesM   = fst <$> split
    dirsM    = snd <$> split


fileAsLines :: FilePath -> IO [String]
fileAsLines file = splitOn "\n" <$> readFile file


writeLines :: (Todo, [String]) -> IO ()
writeLines (Todo p _ _ _, lines) =
  do writeFile tempName (intercalate "\n" lines)
     removeFile p
     renameFile tempName p
  where
    tempName = p <> ".ado"
