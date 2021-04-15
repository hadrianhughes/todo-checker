module Config where

import Data.Set as Set
import Data.Map as Map


data Action = Review | Report | Help deriving (Show)

data ParseError = ParseError String

data AppContext = AppContext { path  :: FilePath
                             , files :: Map FilePath [String] } deriving (Show)

data FileType = Haskell | JavaScript deriving (Show, Ord, Eq)

data Todo = Todo FilePath FileType (Integer,Integer) [String] deriving (Show)

data CommentToken = CommentToken String (Maybe String)



-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = Set.fromList ["node_modules"]


fileTypeFromExt :: String -> Maybe FileType
fileTypeFromExt e =
  case e of
    ".hs" -> Just Haskell
    ".js" -> Just JavaScript
    _     -> Nothing


getTokens :: FileType -> (CommentToken, Maybe CommentToken)
getTokens ft =
  case ft of
    Haskell    -> (smplCmnt "--", Nothing)
    JavaScript -> (smplCmnt "//", Just $ fllCmnt "/*" "*/")


smplCmnt :: String -> CommentToken
smplCmnt t = CommentToken t Nothing


fllCmnt :: String -> String -> CommentToken
fllCmnt t1 t2 = CommentToken t1 (Just t2)


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"


argMappings :: Map String String
argMappings = Map.fromList [("-p", "path")]
