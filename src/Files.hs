module Files
  ( getFiles
  ) where

import System.Directory
import Control.Monad
import qualified Data.Set as Set

import Config

isRealObject :: (String -> IO Bool) -> String -> String -> IO Bool
isRealObject _ _ "."  = return False
isRealObject _ _ ".." = return False
isRealObject f path name = f (path ++ "/" ++ name)


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
  directories <- let f1 = filterM (isRealObject doesDirectoryExist fullDir)
                     f2 = filter isAllowedDir
                 in f2 <$> f1 contents

  fs <- let f1 = filterM (isRealObject doesFileExist fullDir)
            f2 = filter isAllowedFile
        in f2 <$> f1 contents

  fd <- if length directories == 0
           then return []
           else mapM (getFiles' fullDir . (++ "/")) directories

  let files = fs ++ (concat fd)

  return $ map (subDir ++) files
