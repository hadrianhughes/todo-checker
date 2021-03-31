module Config
  ( ignoreDirectories
  ) where

import Data.Set

ignoreDirectories :: Set String
ignoreDirectories = fromList [".git", "node_modules"]
