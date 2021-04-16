module Main (main) where

import System.Environment
import System.IO
import Control.Monad.State
import Debug.Trace

import Config
import InputOutput
import Utils
import Branches


getStdinPaths :: IO (Maybe [String])
getStdinPaths =
  do buffState <- hGetBuffering stdin
     if buffState == LineBuffering
        then return Nothing
        else do contents <- getContents
                return $ Just (lines contents)


initialise :: IO (Action, AppContext)
initialise =
  do args <- getArgs
     piped <- getStdinPaths

     case parseArgs args of
        Right (action, options) ->
          do ctx <- setupContext options piped
             return (action, ctx)
        Left  (ParseError e)    -> error e


main :: IO [()]
main =
  do (a,ctx) <- initialise
     evalStateT (branchAction a) ctx
