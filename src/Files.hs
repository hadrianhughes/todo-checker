module Files
  ( collectFiles
  , findTodos
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


data Todo = Todo FilePath Integer String deriving (Show)


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


collectFiles :: Context -> IO [FilePath]
collectFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, isHidden]


findTodos :: (FilePath, String) -> [Todo]
findTodos (file, txt) = [Todo file i l | (i,l) <- zip [1..] lines, isTodo l]
  where
    lines = splitOn "\n" txt


isTodo :: String -> Bool
isTodo = rgxCheck "^ *-- *(todo|TODO)"
