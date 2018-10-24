{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Aeson

import Lib

input = "{\"rentDates\":[\"2017-11-19T05:00:00.000Z\",\"2017-11-20T05:00:00.000Z\",\"2017-11-21T05:00:00.000Z\"],\"car\":{\"model\":\"Cherato\",\"type\":\"sport\"},\"membership\":false,\"age\":24}"

main :: IO ()
main = hspec $ do
  putStrLn "Not implemented"
