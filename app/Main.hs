module Main where

import System.Environment
import System.Directory

import Files
import Input
import Utils


initialise :: IO Context
initialise = do
  args <- getArgs
  setupContext (parseArgs args)

main :: IO [()]
main = initialise >>= getFiles >>= mapM putStrLn
