module Files
  ( getFiles
  ) where

import System.Directory.Recursive
import System.FilePath
import Control.Monad.State
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


getFiles :: Context -> IO [String]
getFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, isHidden]
