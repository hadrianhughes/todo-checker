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
import Debug.Trace

import Config
import Utils


isHidden :: FilePath -> Bool
isHidden ('.':_) = True
isHidden _       = False


isIgnored :: FilePath -> Bool
isIgnored name = Set.member name ignoredDirectories


findTodos :: FilePath -> [String] -> Maybe [Todo]
findTodos file txt =
  case ftM of
    Just ft -> mapM ((uncurry $ todoFromTo file txt)
                         . (\i -> (i, lastCommentLine ft $ drop (fromIntegral $ i+1) txt))
                         . fst)
               $ filter (isTodo ft . snd) (zip [0..] txt)
    Nothing -> Nothing
  where
    ftM = fileTypeFromExt $ takeExtension file


todoFromTo :: FilePath -> [String] -> Integer -> Integer -> Maybe Todo
todoFromTo file txt i j =
  case fileTypeFromExt $ takeExtension file of
    Just t  -> Just $ Todo file t (i+1, i+j+1) (slice i (i+j) txt)
    Nothing -> Nothing


lastCommentLine :: FileType -> [String] -> Integer
lastCommentLine _ [] = 0
lastCommentLine ft (l:ls)
  | shouldCount l = 1 + (lastCommentLine ft ls)
  | otherwise     = 0
  where
    shouldCount = combinePreds [isLineComment ft, (not . isTodo ft)]


isTodo :: FileType -> String -> Bool
isTodo ft xs = isLineTodo ft xs


isLineTodo :: FileType -> String -> Bool
isLineTodo ft = rgxCheck ("^.*" <> t <> " *(todo|TODO)")
  where
    (CommentToken t _,_) = getTokens ft


isLineComment :: FileType -> String -> Bool
isLineComment ft = rgxCheck ("^ *" <> t)
  where
    (CommentToken t _,_) = getTokens ft


removeTodoLines :: [(Todo, [String])] -> [(Todo, [String])]
removeTodoLines [] = []
removeTodoLines ((todo,ls):xs) =
  let (Todo _ ft (l1,l2) _) = todo
      (before,inside,after) = split3At (l1-1) (l2-2) ls
  in (todo, before ++ (stripComments ft inside) ++ after) : removeTodoLines xs


stripComments :: FileType -> [String] -> [String]
stripComments ft xs =
  case mapM (matchRegex (mkRegex "(.*)--.*$")) endComments of
    Just xs' -> concat xs'
    Nothing  -> []
  where
    endComments = filter (not . isLineComment ft) xs


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
writeLines (Todo p _ _ _, lines) =
  do writeFile tempName (intercalate "\n" lines)
     removeFile p
     renameFile tempName p
  where
    tempName = p <> ".ado"
