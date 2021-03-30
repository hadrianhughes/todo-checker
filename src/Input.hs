module Input
  ( getFiles
  ) where

import System.Directory
import Control.Monad

getFiles :: String -> IO [String]
getFiles dir = do
  files <- getDirectoryContents dir >>= filterM doesFileExist
  return $ map ((dir ++ "/") ++) files
