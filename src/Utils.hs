module Utils where

import System.Directory
import Control.Monad.State
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


modifyCtxFiles :: [(FilePath, String)] -> AppContext -> AppContext
modifyCtxFiles ps ctx = ctx {files = Map.fromList ps}
