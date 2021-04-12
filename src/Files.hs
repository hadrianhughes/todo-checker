module Files
  ( collectFiles
  , findTodos
  , fileAsLines
  , removeTodoLines
  , writeLines
  , Todo (Todo)
  ) where

import System.Directory
import System.Directory.Recursive
import System.FilePath
import Control.Monad.State
import Text.Regex
import Data.List
import Data.List.Split
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False


isIgnored :: FilePath -> Bool
isIgnored name = Set.notMember name ignoredDirectories


findTodos :: (FilePath, [String]) -> [Todo]
findTodos (file, txt) = [Todo file i l | (i,l) <- zip [1..] txt, isTodo l]


isTodo :: String -> Bool
isTodo = rgxCheck "^ *-- *(todo|TODO)"


removeTodoLines :: [(Todo, [String])] -> [(Todo, [String])]
removeTodoLines [] = []
removeTodoLines ((todo,ls):xs) = let (Todo _ i _) = todo
                                 in (todo, removeFromList (i-1) ls) : xs


-- Side effects

collectFiles :: AppContext -> IO [FilePath]
collectFiles ctx = getDirFiltered (return . preds . takeFileName) (path ctx)
  where
    preds = combinePreds [isIgnored, not . isHidden]


fileAsLines :: FilePath -> IO [String]
fileAsLines file = splitOn "\n" <$> readFile file


writeLines :: (Todo, [String]) -> IO ()
writeLines (Todo p _ _, lines) =
  do writeFile tempName (intercalate "\n" lines)
     removeFile p
     renameFile tempName p
  where
    tempName = p <> ".ado"
