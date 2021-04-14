module Files where

import System.Directory
import System.Directory.Recursive
import System.FilePath
import Control.Monad.State
import Control.Applicative
import Text.Regex
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Set as Set

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False


isIgnored :: FilePath -> Bool
isIgnored name = Set.member name ignoredDirectories


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
    shouldCount = combinePreds [isLineComment, (not . isTodo)]


isTodo :: String -> Bool
isTodo = rgxCheck "^.*-- *(todo|TODO)"


isLineComment :: String -> Bool
isLineComment = rgxCheck "^ *--"


removeTodoLines :: [(Todo, [String])] -> [(Todo, [String])]
removeTodoLines [] = []
removeTodoLines ((todo,ls):xs) = let (Todo _ (l1,l2) _) = todo
                                 in (todo, stripComments ls) : removeTodoLines xs


stripComments :: [String] -> [String]
stripComments xs =
  case traverse (matchRegex (mkRegex "(.*)--.*$")) endComments of
    Just xs' -> concat xs'
    Nothing  -> []
  where
    endComments = filter (not . isLineComment) xs


-- Side effects

collectFiles :: FilePath -> IO [FilePath]
collectFiles p = (liftA2 (++)) filesM (concat <$> (mapM collectFiles =<< dirsM))
  where
    preds    = combinePreds [not . isIgnored, not . isHidden]
    filtered = (map ((appendOnce '/' p) <>)) <$> (filter preds) <$> listDirectory p
    split    = partitionM doesFileExist =<< filtered
    filesM   = fst <$> split
    dirsM    = snd <$> split


fileAsLines :: FilePath -> IO [String]
fileAsLines file = splitOn "\n" <$> readFile file


writeLines :: (Todo, [String]) -> IO ()
writeLines (Todo p _ _, lines) =
  do writeFile tempName (intercalate "\n" lines)
     removeFile p
     renameFile tempName p
  where
    tempName = p <> ".ado"
