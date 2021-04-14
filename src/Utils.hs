module Utils where

import System.Directory
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Text.Regex
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Config


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds ps x = all (\p -> p x) ps


rgxCheck :: String -> String -> Bool
rgxCheck rgx xs = isJust $ matchRegex (mkRegex rgx) xs


modifyCtxFiles :: [(FilePath, [String])] -> AppContext -> AppContext
modifyCtxFiles ps ctx = ctx {files = Map.fromList ps}


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


-- Side effects

setupContext :: Map String String -> IO AppContext
setupContext args =
  case Map.lookup "path" args of
    Just p  -> return $ AppContext {path = p, files = Map.empty}
    Nothing -> return $ AppContext {path = "./", files = Map.empty}
