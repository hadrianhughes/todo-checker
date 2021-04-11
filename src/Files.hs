module Files
  ( collectFiles
  , findTodos
  , fileAsLines
  , Todo (Todo)
  ) where

import System.Directory.Recursive
import System.FilePath
import Control.Monad.State
import Text.Regex
import Data.List.Split
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


collectFiles :: AppContext -> IO [FilePath]
collectFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, not . isHidden]


fileAsLines :: FilePath -> IO [String]
fileAsLines file = splitOn "\n" <$> readFile file


findTodos :: (FilePath, [String]) -> [Todo]
findTodos (file, txt) = [Todo file i l | (i,l) <- zip [1..] txt, isTodo l]


isTodo :: String -> Bool
isTodo = rgxCheck "^ *-- *(todo|TODO)"
