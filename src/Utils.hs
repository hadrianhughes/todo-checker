module Utils
  ( combinePreds
  ) where


combinePreds :: [(a -> Bool)] -> a -> Bool
combinePreds ps x = all (\p -> p x) ps
