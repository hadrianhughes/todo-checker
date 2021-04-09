module Main where

import System.Environment
import System.Directory

import Files
import InputOutput
import Utils
import Config


initialise :: IO AppContext
initialise = do
  args <- getArgs

  case parseArgs args of
    Right (action, options) -> setupContext options
    Left  (ParseError e)    -> error e


main :: IO [()]
main = do
  ctx <- initialise
  files <- collectFiles ctx
  contents <- mapM readFile files

  let todos = concat $ map findTodos $ zip files contents
  states <- mapM checkTodoDone todos

  let completed = [t | (t,s) <- zip todos states, s]

  mapM (putStrLn . show) completed
