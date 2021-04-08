module Config
  ( ignoredDirectories
  ) where

import Data.Set

-- TODO: Example
ignoredDirectories :: Set String
ignoredDirectories = fromList ["node_modules"]
