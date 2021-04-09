module Config
  ( ignoredDirectories
  , checkCompletedString
  ) where

import Data.Set

-- TODO: Example
-- Directories starting with . don't need to be added
ignoredDirectories :: Set String
ignoredDirectories = fromList ["node_modules"]


checkCompletedString :: String
checkCompletedString = "Is this todo completed? (y/N)"
