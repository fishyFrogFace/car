module Main where

import Lib
import Parse

main :: IO ()
main = do
    content <- readFile "src/content.txt"
    print $ map parseInput (lines content)
