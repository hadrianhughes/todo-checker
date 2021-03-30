module Input
  ( getFiles
  ) where

import System.Directory
import Control.Monad

getFiles :: String -> IO [String]
getFiles dir = getDirectoryContents dir >>= filterM doesFileExist
-- getFiles dir = do
  -- contents <- getDirectoryFiles dir
  -- filterM doesFileExist contents
