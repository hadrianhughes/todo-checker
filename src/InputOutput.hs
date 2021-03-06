module InputOutput where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import Utils
import Files
import Config



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
displayTodo (Todo file _ (l1,l2) comments) = file <> "\n" <> (intercalate "\n" . map showTuple . zip [l1..l2]) comments <> "\n"
  where
    showTuple = \(l,c) -> show l <> ": " <> c



-- Side effects

parseArgs :: [String] -> Either ParseError (Action, Map String String)
parseArgs [] = Left (ParseError "No command given")
parseArgs (a:xs) =
  case parseAction a of
    Just action' -> Right (action', parseOptions xs)
    Nothing      -> Left (ParseError $ "Command " <> a <> " not recognised")


checkTodoDone :: Todo -> IO Bool
checkTodoDone todo = putStrLn (displayTodo todo)
                     >> putStrLn checkCompletedString
                     >> getLine
                     >>= (\x -> return $ x == "y" || x == "Y")
