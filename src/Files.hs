module Files
  ( getFiles
  ) where

import System.Directory.Recursive
import System.FilePath
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


getFiles :: FilePath -> IO [String]
getFiles path = getDirFiltered (return . preds . takeFileName) path
  where
    preds = combinePreds [isIgnored, isHidden]
