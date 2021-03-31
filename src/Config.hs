module Config
  ( ignoredDirectories
  ) where

import Data.Set

ignoredDirectories :: Set String
ignoredDirectories = fromList ["node_modules"]
