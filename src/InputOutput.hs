module InputOutput
  ( parseArgs
  , displayTodo
  , checkTodoDone
  ) where

import Data.Map as Map
import Data.Set as Set
import Debug.Trace

import Utils
import Files
import Config


parseArgs :: [String] -> Map String String
parseArgs ("-p":path:xs) = Map.fromList (("path", path) : (Map.toList $ parseArgs xs))
parseArgs [] = Map.empty
parseArgs (_:xs) = parseArgs xs


displayTodo :: Todo -> String
displayTodo (Todo file line comment) = file ++ ":" ++ (show line) ++ " " ++ comment


checkTodoDone :: Todo -> IO Bool
checkTodoDone todo = do
  putStrLn $ displayTodo todo
  putStrLn checkCompletedString
  x <- getLine
  return $ x == "y" || x == "Y"
