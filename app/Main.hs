module Main (main) where

import System.Environment
import System.IO
import Control.Monad.State

import Config
import InputOutput
import Utils
import Branches


getStdin :: IO (Maybe [String])
getStdin =
  do buffState <- hGetBuffering stdin
     if buffState == LineBuffering
        then return Nothing
        else do contents <- getContents
                return $ Just (lines contents)


initialise :: IO (Action, AppContext)
initialise =
  do args <- getArgs

     case parseArgs args of
        Right (action, options) ->
          do ctx <- setupContext options
             return (action, ctx)
        Left  (ParseError e)    -> error e


main :: IO [()]
main =
  do (a,ctx) <- initialise
     evalStateT (branchAction a) ctx
