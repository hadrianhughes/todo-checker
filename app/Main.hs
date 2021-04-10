module Main (main) where

import System.Environment

import Config
import InputOutput
import Utils
import Branches


initialise :: IO (Action, AppContext)
initialise =
  do args <- getArgs

     case parseArgs args of
        Right (action, options) ->
          do ctx <- setupContext options
             return (action, ctx)
        Left  (ParseError e)    -> error e


main :: IO [()]
main = (uncurry branchAction) =<< initialise
