module Files
  ( getFiles
  ) where

import System.Directory.Recursive
import System.FilePath
import qualified Data.Set as Set

import Config


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds [] _ = True
combinePreds ps x = all (\p -> p x) ps


getFiles :: FilePath -> IO [String]
getFiles path = getDirFiltered (return . preds . takeFileName) path
  where
    preds = combinePreds [isIgnored, isHidden]
