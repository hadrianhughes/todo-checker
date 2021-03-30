module Main where

import System.Environment
import System.Directory
import Input

main :: IO [()]
main = do
  dir <- getCurrentDirectory
  files <- getFiles dir
  mapM putStrLn files
