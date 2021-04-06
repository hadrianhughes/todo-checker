module InputOutput
  ( parseArgs
  ) where

import Utils
import Data.Map as Map
import Data.Set as Set
import Debug.Trace


parseArgs :: [String] -> Map String String
parseArgs ("-p":path:xs) = Map.fromList (("path", path) : (Map.toList $ parseArgs xs))
parseArgs [] = Map.empty
parseArgs (_:xs) = parseArgs xs
