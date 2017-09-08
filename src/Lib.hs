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

--dummy function, since creating the price information is not implemented yet
calcTotal :: [String] -> String
calcTotal input = show . encode $ PriceInfo 1 2 3 4
