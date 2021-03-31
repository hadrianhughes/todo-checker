module Config
  ( ignoreDirectories
  , fileRgx
  ) where

import Data.Set
import Text.Regex

ignoreDirectories :: Set String
ignoreDirectories = fromList ["node_modules"]

fileRgx :: Regex
fileRgx = mkRegex "^.*/\\..*$"
