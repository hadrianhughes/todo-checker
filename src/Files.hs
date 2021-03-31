module Files
  ( getFiles
  ) where

import System.Directory
import Control.Monad
import qualified Data.Set as Set

import Config


isAllowedDir :: String -> Bool
isAllowedDir ('.':_) = False
isAllowedDir dir     = Set.notMember dir ignoreDirectories


isAllowedFile :: String -> Bool
isAllowedFile ('.':_) = False
isAllowedFile _       = True


getFiles :: String -> IO [String]
getFiles dir = getFiles' dir "/"


getFiles' :: String -> String -> IO [String]
getFiles' dir subDir = do
  let fullDir = dir ++ subDir

  contents <- getDirectoryContents fullDir
  directories <- let f1 = filterM (doesDirectoryExist . ((fullDir ++ "/") ++))
                     f2 = filter isAllowedDir
                 in f2 <$> f1 contents

  fs <- let f1 = filterM (doesFileExist . ((fullDir ++ "/") ++))
            f2 = filter isAllowedFile
        in f2 <$> f1 contents

  fd <- if length directories == 0
           then return []
           else mapM (getFiles' fullDir . (++ "/")) directories

  let files = let files' = fs ++ (concat fd)
              in if subDir == "/"
                    then files'
                    else map (subDir ++) files'

  return files
