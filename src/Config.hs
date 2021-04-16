module Config where

import Data.List
import Data.Set as Set
import Data.Map as Map


data Action = Review | Report | Help deriving (Show)

data ParseError = ParseError String

data AppContext = AppContext { path  :: FilePath
                             , files :: [(FilePath, [String])] } deriving (Show)

data CommentStyle = Haskell | CLang | Python | Bash | Lisp | FSharp | Ruby deriving (Show, Ord, Eq)

data Todo = Todo FilePath CommentStyle (Integer,Integer) [String] deriving (Show)

data CommentToken = CommentToken String (Maybe String)



-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = Set.fromList ["node_modules"]


fileTypeFromExt :: String -> Maybe CommentStyle
fileTypeFromExt [] = Nothing
fileTypeFromExt e = fst <$> find (Set.member (tail e) . snd) extMappings
  where
    extMappings = [ (Haskell, Set.fromList ["hs"])
                  , (CLang,   Set.fromList ["c", "js", "ts", "tsx", "jsx", "java", "swift", "m", "h", "kt", "cpp", "go", "rs", "cs", "scala", "sc"])
                  , (Python,  Set.fromList ["py"])
                  , (Bash,    Set.fromList ["sh", "r"])
                  , (Lisp,    Set.fromList ["clj", "cljs", "lisp", "cl", "lsp"])
                  , (FSharp,  Set.fromList ["fs"])
                  , (Ruby,    Set.fromList ["rb"])]


getTokens :: CommentStyle -> (CommentToken, Maybe CommentToken)
getTokens ft =
  case ft of
    Haskell -> (smplCmnt "--", Nothing)
    CLang   -> (smplCmnt "//", Just $ fllCmnt "/*" "*/")
    Python  -> (smplCmnt "#",  Just $ fllCmnt "\"\"\"" "\"\"\"")
    Bash    -> (smplCmnt "#",  Nothing)
    Lisp    -> (smplCmnt ";",  Just $ fllCmnt "#|" "|#")
    FSharp  -> (smplCmnt "//", Just $ fllCmnt "(*" "*)")
    Ruby    -> (smplCmnt "#",  Just $ fllCmnt "=begin" "=end")


smplCmnt :: String -> CommentToken
smplCmnt t = CommentToken t Nothing


fllCmnt :: String -> String -> CommentToken
fllCmnt t1 t2 = CommentToken t1 (Just t2)


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"


argMappings :: Map String String
argMappings = Map.fromList [("-p", "path")]
