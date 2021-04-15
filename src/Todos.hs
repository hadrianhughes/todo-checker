module Todos where

import System.FilePath
import Text.Regex

import Config
import Utils


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
      (before,inside,after) = split3At (l1-1) (l2-1) ls
  in (todo, before ++ (stripComments ft inside) ++ after) : removeTodoLines xs


stripComments :: FileType -> [String] -> [String]
stripComments ft xs =
  case mapM (matchRegex (mkRegex ("(.*))" <> t <> ".*$"))) endComments of
    Just xs' -> concat xs'
    Nothing  -> []
  where
    endComments          = filter (not . isLineComment ft) xs
    (CommentToken t _,_) = getTokens ft
