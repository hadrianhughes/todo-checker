module Branches (branchAction) where

import Control.Applicative
import Control.Monad.State

import Config
import Files
import InputOutput


handleCollection :: AppContext -> IO [Todo]
handleCollection ctx =
  do files <- collectFiles ctx
     contents <- mapM readFile files
     return $ concat
            $ map findTodos
            $ zip files contents


review :: AppContext -> IO [()]
review ctx =
  do todos <- handleCollection ctx
     states <- mapM checkTodoDone todos
     mapM (putStrLn . show) [t | (t,s) <- zip todos states, s]


report :: StateT AppContext IO [()]
report =
  do ctx <- get
     let todos = handleCollection ctx
     return $ liftIO $ mapM_ (putStrLn . displayTodo) =<< todos


help :: StateT AppContext IO [()]
help = return $ liftIO $ mapM_ putStrLn ["Help text"]


branchAction :: Action -> StateT AppContext IO [()]
branchAction a =
  case a of
    Review -> review
    Report -> report
    Help   -> help
