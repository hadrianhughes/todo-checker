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


data Todo = Todo Integer String deriving (Show)


isHidden :: FilePath -> Bool
isHidden ('.':_) = False
isHidden _       = True


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


collectFiles :: Context -> IO [String]
collectFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, isHidden]


findTodos :: String -> [Todo]
findTodos txt = [Todo i l | (i,l) <- zip [1..] lines, isTodo l]
  where
    lines = splitOn "\n" txt


isTodo :: String -> Bool
isTodo xs = rgxCheck "^ *-- *(todo|TODO)" xs
