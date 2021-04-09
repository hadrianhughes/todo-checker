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



parseArgs :: [String] -> Either ParseError (Action, Map String String)
parseArgs (a:xs) =
  case parseAction a of
    Just action' -> Right (action', parseOptions xs)
    Nothing      -> Left (ParseError $ "Command " <> a <> " not recognised")


parseAction :: String -> Maybe Action
parseAction a =
  case a of
    "report" -> Just Report
    "review" -> Just Review
    _        -> Nothing


parseOptions :: [String] -> Map String String
parseOptions (op:x:xs) =
  case Map.lookup op argMappings of
    Just name -> Map.insert name x (parseOptions xs)
    Nothing   -> parseOptions xs

parseOptions [] = Map.empty
parseOptions (_:xs) = parseOptions xs


displayTodo :: Todo -> String
displayTodo (Todo file line comment) = file <> ":" <> (show line) <> " " <> comment


checkTodoDone :: Todo -> IO Bool
checkTodoDone todo = do
  putStrLn $ displayTodo todo
  putStrLn checkCompletedString
  x <- getLine
  return $ x == "y" || x == "Y"
