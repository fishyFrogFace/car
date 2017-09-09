{-# LANGUAGE OverloadedStrings, DeriveGeneric, NamedFieldPuns #-}
module Lib
    ( Car(..)
    , PriceInfo(..)
    , RentalInfo(..)
    , carPrice
    , calcTotal
    , toRentalInfo
    , validateRental
    , carTotal
    , dayDiscount
    , afterDiscounts
    ) where

import Data.Aeson
import GHC.Generics
import Text.Read

data Car
    = Small
    | Sport
    | SUV
    deriving (Show, Eq)

data RentalInfo = RentalInfo {
    dates :: [Int],
    carType :: Car,
    member :: Bool,
    age :: Int
} deriving (Show, Eq)

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

carTotal :: Car -> [Int] -> Int
carTotal _ [] = 0
carTotal car (x:xs)
    |x < 6     = (dayPrice - (dayPrice `div` 10)) + (carTotal car xs)
    |otherwise = dayPrice + (carTotal car xs) 
   where
    dayPrice = carPrice car

dayDiscount :: [Int] -> Float
dayDiscount [] = 0
dayDiscount dates
    |days < 3   = 1
    |days >= 11 = 0.85
    |days >= 6  = 0.9
    |otherwise  = 0.95
   where
    days = length dates

afterDiscounts :: Car -> [Int] -> Bool -> Float
afterDiscounts car days mem
    |mem = tot*0.95
    |otherwise = tot
   where
    tot = fromIntegral (carTotal car days) * (dayDiscount days)

toRentalInfo :: ([Maybe Int], [String]) -> Maybe RentalInfo
toRentalInfo (days, [car, member, age])
    |Nothing `elem` days = Nothing
    |c == Nothing        = Nothing
    |m == Nothing        = Nothing
    |a == Nothing        = Nothing
    |otherwise           = Just $ RentalInfo (map getVal days) (getVal c) (getVal m) (getVal a)
   where
    c = toCar car
    m = toMember member
    a = readMaybe age :: Maybe Int
    getVal (Just val) = val

toCar :: String -> Maybe Car
toCar car
    |car == "small" = Just Small
    |car == "sport" = Just Sport
    |car == "suv"   = Just SUV
    |otherwise      = Nothing 

toMember :: String -> Maybe Bool
toMember member
    |member == "false" = Just False
    |member == "true"  = Just True
    |otherwise         = Nothing

is18 :: RentalInfo -> Bool
is18 (RentalInfo {age})
    |age >= 18 = True
    |otherwise = False

validateRental :: ([Maybe Int], [String]) -> Maybe RentalInfo
validateRental p
    |mRental == Nothing = Nothing
    |is18 r             = mRental
    |otherwise          = Nothing
   where
    mRental = toRentalInfo p
    rental (Just r) = r
    r = rental mRental

--dummy function, since creating the price information is not implemented yet
calcTotal :: ([Maybe Int], [String]) -> String
calcTotal input = case validateRental input of
                    Just rental -> show . encode $ PriceInfo 1 2 3 4
                    Nothing     -> "The rental was not valid or renter is under 18"
