module Utils where

import System.Directory
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Text.Regex
import Data.Map as Map
import Data.Set as Set

import Config


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds ps x = all (\p -> p x) ps


setupContext :: Map String String -> IO AppContext
setupContext args =
  case Map.lookup "path" args of
    Just p  -> return $ AppContext {path = p, files = Map.empty}
    Nothing ->
      do dir <- getCurrentDirectory
         return $ AppContext {path = dir, files = Map.empty}


rgxCheck :: String -> String -> Bool
rgxCheck rgx xs = isJust $ matchRegex (mkRegex rgx) xs


modifyCtxFiles :: [(FilePath, [String])] -> AppContext -> AppContext
modifyCtxFiles ps ctx = ctx {files = Map.fromList ps}


removeFromList :: Integer -> [a] -> [a]
removeFromList i xs = Prelude.take (fromIntegral i) xs ++ Prelude.drop (fromIntegral i + 1) xs


fileFromTodo :: Map FilePath [String] -> Todo -> [String]
fileFromTodo files (Todo path _ _) =
  case Map.lookup path files of
    Just xs -> xs
    Nothing -> []


fzip :: Applicative f => f [a] -> f [b] -> f [(a,b)]
fzip = liftA2 zip
