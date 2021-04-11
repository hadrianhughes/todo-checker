module Branches (branchAction) where

import Control.Applicative
import Control.Monad.State

import Config
import Files
import InputOutput

handleCollection :: StateT AppContext IO [Todo]
handleCollection =
  do let files = collectFiles get
         contents = mapM readFile =<< files

     liftIO $ concat <$> map findTodos <$> (liftA2 zip) files contents


review :: StateT AppContext IO [()]
review =
  do ctx <- get

     let todos = evalStateT handleCollection ctx
         states = mapM checkTodoDone =<< todos
         pairings = (liftA2 zip) todos states

     liftIO $ mapM (putStrLn . show) =<< (map fst . filter snd) <$> pairings


report :: StateT AppContext IO [()]
report =
  do ctx <- get
     let todos = evalStateT handleCollection ctx
     liftIO $ mapM (putStrLn . displayTodo) =<< todos


help :: StateT AppContext IO [()]
help = liftIO $ mapM putStrLn ["Help text"]


branchAction :: Action -> StateT AppContext IO [()]
branchAction a =
  case a of
    Review -> review
    Report -> report
    Help   -> help
