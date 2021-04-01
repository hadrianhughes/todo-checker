module Utils where

import System.Directory
import Data.Map as Map
import Data.Set as Set


data Context = Context {path :: FilePath} deriving (Show)


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds ps x = all (\p -> p x) ps


setupContext :: Map String String -> IO Context
setupContext args =
  case Map.lookup "path" args of
    Just p  -> return $ Context {path = p}
    Nothing -> do
      dir <- getCurrentDirectory
      return $ Context {path = dir}
