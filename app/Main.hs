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
main = do
  ctx <- initialise
  files <- collectFiles ctx
  contents <- mapM readFile files

  let todos = concat $ map findTodos $ zip files contents

  mapM putStrLn (map displayTodo todos)
