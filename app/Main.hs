module Main where

import Prelude hiding (readFile, lines)
import Lib
import Data.ByteString.Lazy (ByteString(..), readFile)
import Data.ByteString.Lazy.Char8 (lines)

main :: IO ()
main = do 
       content <- readFile "src/content.txt"
       print $ map calcTotal $ lines content
