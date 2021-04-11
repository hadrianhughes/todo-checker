module Branches (branchAction) where

import Control.Monad.State

import Config
import Files
import InputOutput
import Utils


handleCollection :: StateT AppContext IO [Todo]
handleCollection =
  do ctx <- get
     let files = collectFiles ctx
         contents = mapM readFile =<< files
         fzip = liftA2 zip

     modify =<< liftIO (modifyCtxFiles <$> fzip files contents)

     liftIO $ concat <$> map findTodos <$> fzip files contents


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
