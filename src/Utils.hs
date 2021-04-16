module Utils where

import System.Directory
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Text.Regex
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Config


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds ps x = all (\p -> p x) ps


rgxCheck :: String -> String -> Bool
rgxCheck rgx xs = isJust $ matchRegex (mkRegex rgx) xs


slice :: Integer -> Integer -> [a] -> [a]
slice from to xs = take (fromIntegral $ to - from + 1) (drop (fromIntegral from) xs)


fzip :: Applicative f => f [a] -> f [b] -> f [(a,b)]
fzip = liftA2 zip


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f [] = return ([],[])
partitionM f (x:xs) =
  do res <- f x
     (as,bs) <- partitionM f xs
     return ([x | res] ++ as, [x | not res] ++ bs)


appendOnce :: Char -> String -> String
appendOnce c xs =
  case xs of
    (_:c:[]) -> []
    _        -> xs <> [c]


fileFromTodo :: [(FilePath, [String])] -> Todo -> [String]
fileFromTodo files (Todo path _ _ _) =
  case find ((== path) . fst) files of
    Just x  -> snd x
    Nothing -> []


split3At :: Integer -> Integer -> [a] -> ([a],[a],[a])
split3At i j xs = (c1,c2,c3)
  where
    (c1,xs') = splitAt (fromIntegral i) xs
    (c2,c3)  = splitAt (fromIntegral j) xs'


-- Side effects

setupContext :: Map String String -> Maybe [FilePath] -> IO AppContext
setupContext args filesM =
  case Map.lookup "path" args of
    Just p  -> return $ AppContext {path = p, files = files}
    Nothing -> return $ AppContext {path = "./", files = files}
  where
    files = case filesM of
              Just fs -> map (\f -> (f :: FilePath, [])) fs
              Nothing -> []
