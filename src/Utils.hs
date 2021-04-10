module Utils where

import System.Directory
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
    Just p  -> return $ AppContext {path = p}
    Nothing ->
      do dir <- getCurrentDirectory
         return $ AppContext {path = dir}


rgxCheck :: String -> String -> Bool
rgxCheck rgx xs = isJust $ matchRegex (mkRegex rgx) xs
