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


findTodos :: FilePath -> [String] -> [Todo]
findTodos file txt =
  [todoFromTo i $ lastCommentLine (drop (fromIntegral $ i+1) txt) | (i,l) <- zip [0..] txt, isTodo l]
    where
      todoFromTo = (\i j -> Todo file (i+1,i+j+1) (slice i (i+j) txt))


lastCommentLine :: [String] -> Integer
lastCommentLine [] = 0
lastCommentLine (l:ls)
  | shouldCount l = 1 + (lastCommentLine ls)
  | otherwise     = 0
  where
    shouldCount = combinePreds [isComment, (not . isTodo)]


isTodo :: String -> Bool
isTodo = rgxCheck "^ *-- *(todo|TODO)"


isComment :: String -> Bool
isComment = rgxCheck "^ *--"


removeTodoLines :: [(Todo, [String])] -> [(Todo, [String])]
removeTodoLines [] = []
removeTodoLines ((todo,ls):xs) = let (Todo _ (l1,l2) _) = todo
                                 in (todo, removeSlice (l1-1) (l2-1) ls) : removeTodoLines xs


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
