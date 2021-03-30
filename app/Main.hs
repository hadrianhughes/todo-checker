module Main where

import System.Environment
import System.Directory
import Lib

main :: IO ()
main = do
  wd <- getCurrentDirectory
  putStrLn wd
