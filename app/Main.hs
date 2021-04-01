module Main where

import System.Environment
import System.Directory
import Files

collectFiles :: IO [FilePath]
collectFiles = getCurrentDirectory >>= getFiles

main :: IO [()]
main = collectFiles >>= mapM putStrLn
