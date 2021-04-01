module Main where

import System.Environment
import System.Directory

import Files
import Input
import Utils

main :: IO [()]
-- main = collectFiles >>= mapM putStrLn
main = do
  args <- getArgs
  ctx <- setupContext (parseArgs args)

  files <- getFiles ctx

  mapM putStrLn files
