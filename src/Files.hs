module Files
  ( collectFiles
  , findTodos
  , Todo
  ) where

import System.Directory.Recursive
import System.FilePath
import Control.Monad.State
import Text.Regex
import Data.List.Split
import qualified Data.Set as Set

import Config
import Utils


data Todo = Todo Integer String


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


collectFiles :: Context -> IO [String]
collectFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, isHidden]


findTodos :: String -> [String]
findTodos txt = [l | l <- lines, isTodo l]
  where
    lines = splitOn "\n" txt


isTodo :: String -> Bool
isTodo xs = rgxCheck "^ *-- *(todo|TODO)" xs
