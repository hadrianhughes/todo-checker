module Branches (branchAction) where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Debug.Trace

import Config
import Files
import InputOutput
import Utils
import Debug.Trace


handleCollection :: StateT AppContext IO [Todo]
handleCollection =
  do ctx <- get

     let files    = collectFiles (path ctx)
         contents = mapM fileAsLines =<< files

     modify =<< liftIO (modifyCtxFiles <$> fzip files contents)

     todosM <- liftIO $ (map (uncurry findTodos)) <$> fzip files contents

     return $ concat $ catMaybes todosM


review :: StateT AppContext IO [()]
review =
  do ctx <- get
     (todos, ctx') <- liftIO $ runStateT handleCollection ctx
     completed <- liftIO $ map fst <$> filter snd <$> zip todos <$> mapM checkTodoDone todos

     liftIO $ mapM writeLines $ removeTodoLines $ zip completed (map (fileFromTodo $ files ctx') completed)


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
