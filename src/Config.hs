module Config where

import Data.Set as Set
import Data.Map as Map


data Action = Review | Report | Help deriving (Show)

data ParseError = ParseError String

data AppContext = AppContext {path :: FilePath} deriving (Show)

data Todo = Todo FilePath Integer String deriving (Show)



-- TODO: Example
-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = Set.fromList ["node_modules"]


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"


argMappings :: Map String String
argMappings = Map.fromList [("-p", "path")]
