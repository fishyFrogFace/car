{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Lib
    ( Car(..)
    , PriceInfo(..)
    , carPrice
    , calcTotal
    ) where

import Data.Aeson
import GHC.Generics

data Car
    = Small
    | Sport
    | SUV

data PriceInfo = PriceInfo {
    subtotal :: Float,
    insuranceTotal :: Float,
    discountPercentage :: Float,
    totalPayment :: Float
} deriving (Show, Generic)

instance ToJSON PriceInfo

carPrice :: Car -> Int
carPrice carType = case carType of
                    Small -> 40
                    Sport -> 60
                    SUV   -> 100

calcTotal :: IO ()
calcTotal = putStrLn "Not implemented yet"
