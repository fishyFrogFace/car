module Main where

import Prelude hiding (readFile, lines)
import Lib
import Data.ByteString.Lazy (ByteString(..), readFile)
import Data.ByteString.Lazy.Char8 (lines)
import System.Environment (getArgs)

process :: String -> IO ()
process filename = do
  content <- readFile filename
  print $ map calcTotal $ lines content

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> process x
    _   -> putStrLn "You need to provide a filename"
