module Input
  ( getFiles
  ) where

import System.Directory
import Control.Monad
import Debug.Trace

isRealObject :: (String -> IO Bool) -> String -> String -> IO Bool
isRealObject _ _ "."  = return False
isRealObject _ _ ".." = return False
isRealObject f path name = f (path ++ name)

getFiles :: String -> IO [String]
getFiles dir = do
  let fullDir = dir ++ "/"

  contents <- getDirectoryContents dir
  directories <- filterM (isRealObject doesDirectoryExist fullDir) contents

  fs <- filterM (isRealObject doesFileExist fullDir) contents

  deepFs <- if length directories == 0
               then return []
               else mapM getFiles $ map (fullDir ++) directories

  let files = fs ++ (concat deepFs)

  return $ map (fullDir ++) files
