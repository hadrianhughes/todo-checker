module Main where

import System.Environment
import System.Directory

import Files
import InputOutput
import Utils


initialise :: IO Context
initialise = do
  args <- getArgs
  setupContext (parseArgs args)


main :: IO [()]
main = initialise >>= collectFiles >>= mapM readFile >>= mapM (putStrLn . show . findTodos)
