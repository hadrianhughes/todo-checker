module Main (main) where

import System.Environment
import System.Directory
import Data.List

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


review :: AppContext -> IO ()
review ctx = do
  files <- collectFiles ctx
  contents <- mapM readFile files

  let todos = concat $ map findTodos $ zip files contents
  states <- mapM checkTodoDone todos

  let completed = [t | (t,s) <- zip todos states, s]

  putStrLn $ intercalate ", " $ map show completed


report = undefined


help :: AppContext -> IO ()
help _ = putStrLn "Help text"


main :: IO ()
main = do
  (action, ctx) <- initialise

  case action of
    Review -> review ctx
    Report -> report ctx
    Help   -> help ctx
