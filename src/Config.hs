module Config where

import Data.Set


data Action = Review | Report

data ParseError = ParseError String

data AppContext = AppContext {path :: FilePath} deriving (Show)


-- TODO: Example
-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = fromList ["node_modules"]


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"
