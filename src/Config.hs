module Config
  ( ignoreDirectories
  ) where

import Data.Set

ignoreDirectories :: Set String
ignoreDirectories = fromList ["node_modules"]
