module Files
  ( getFiles
  ) where

import System.Directory
import System.Directory.Recursive
import Control.Monad
import qualified Data.Set as Set
import Text.Regex
import Debug.Trace

import Config


isAllowed :: FilePath -> Bool
isAllowed path =
  case m of
    Nothing -> True
    Just _  -> False
  where
    m = matchRegex fileRgx path


getFiles :: FilePath -> IO [String]
getFiles path = getDirFiltered (return . isAllowed) path
