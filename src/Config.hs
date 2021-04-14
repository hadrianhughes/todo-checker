module Config where

import Data.Set as Set
import Data.Map as Map


data Action = Review | Report | Help deriving (Show)

data ParseError = ParseError String

data AppContext = AppContext { path  :: FilePath
                             , files :: Map FilePath [String] } deriving (Show)

data Todo = Todo FilePath (Integer,Integer) [String] deriving (Show)

data CommentToken = CommentToken String (Maybe String)



-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = Set.fromList ["node_modules"]


commentTokens :: Map String [CommentToken]
commentTokens = Map.fromList [(".hs", [smplCmnt "--"]),
                              (".js", [smplCmnt "//", fllCmnt "/*" "*/"])]


smplCmnt :: String -> CommentToken
smplCmnt t = CommentToken t Nothing


fllCmnt :: String -> String -> CommentToken
fllCmnt t1 t2 = CommentToken t1 (Just t2)


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"


argMappings :: Map String String
argMappings = Map.fromList [("-p", "path")]
