module Main where

import System.Environment
import System.Directory

import Files
import InputOutput
import Utils
import Config


initialise :: IO (Action, AppContext)
initialise = do
  args <- getArgs

  case parseArgs args of
    Right (action, options) -> do
      ctx <- setupContext options
      return (action, ctx)
    Left  (ParseError e)    -> error e


review :: AppContext -> IO [()]
review ctx = do
  files <- collectFiles ctx
  contents <- mapM readFile files

  let todos = concat $ map findTodos $ zip files contents
  states <- mapM checkTodoDone todos

  let completed = [t | (t,s) <- zip todos states, s]

  mapM (putStrLn . show) completed


main :: IO [()]
main = do
  (action, ctx) <- initialise

  case action of
    Review -> review ctx
