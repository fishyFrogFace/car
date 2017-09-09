module Main where

import Lib
import Parse

main :: IO ()
main = do
    content <- readFile "src/content.txt"
    let parsed = map parseInput (lines content)
    print $ map calcTotal parsed
