module Branches (branchAction) where

import Control.Applicative

import Config
import Files
import InputOutput


handleCollection :: AppContext -> IO [Todo]
handleCollection ctx = do
  files <- collectFiles ctx
  contents <- mapM readFile files
  return $ concat $ map findTodos $ zip files contents


review :: AppContext -> IO [()]
review ctx = mapM (putStrLn . show) =<< fmap (filter snd) zipped
  where
    todos = handleCollection ctx
    states = mapM checkTodoDone =<< todos
    zipped = (liftA2 zip) todos states


report :: AppContext -> IO [()]
report ctx = mapM (putStrLn . displayTodo) =<< (handleCollection ctx)


help :: AppContext -> IO [()]
help _ = mapM putStrLn ["Help text"]


branchAction :: Action -> AppContext -> IO [()]
branchAction a =
  case a of
    Review -> review
    Report -> report
    Help   -> help
