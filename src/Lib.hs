{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( Car(..)
    , PriceInfo(..)
    , RentalInfo(..)
    , carPrice
    , calcTotal
    , validateRental
    , carTotal
    , dayDiscount
    , afterDiscounts
    , discount
    , insurance
    , toPriceInfo
    , roundTwo
    ) where

import Data.Aeson (FromJSON(..)
                 , ToJSON(..)
                 , decode
                 , encode
                 , (.:)
                 , withObject
                 , pairs
                 , (.=)
                 , object)
import GHC.Generics
import Data.Time.Clock (UTCTime(..), utctDay)
import Data.Time.Calendar (Day(..), dayOfWeek, DayOfWeek(..))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Control.Monad ((=<<))
import Data.ByteString.Lazy.Internal (ByteString(..))

data Model = Dwarfy
           | Halfing
           | Eveo
           | Cherato
           | Vitoro
           | Exploring
           deriving (Show, Eq, Generic)

instance FromJSON Model
instance ToJSON Model

data Car = Car { model :: Model
               , carType :: String
               } deriving (Show, Eq, Generic)

instance FromJSON Car where
  parseJSON = withObject "Car" $ \v -> Car
    <$> v .: "model"
    <*> v .: "type"

instance ToJSON Car where
  toJSON (Car m ct) = object ["model" .= m, "type" .= ct]
  toEncoding (Car m ct) = pairs ("model" .= m <> "type" .= ct)

data RentalInfo = RentalInfo { rentDates :: [UTCTime]
                             , car :: Car
                             , membership :: Bool
                             , age :: Int
                             } deriving (Show, Eq, Generic)

instance FromJSON RentalInfo
instance ToJSON RentalInfo

data PriceInfo = PriceInfo { subtotal :: Int
                           , insuranceTotal :: Double
                           , discountPercentage :: Double
                           , totalPayment :: Double
                           } deriving (Show, Eq, Generic)

instance ToJSON PriceInfo
instance FromJSON PriceInfo

carPrice :: Car -> Int
carPrice car = case carType car of
                    "small" -> 40
                    "sport" -> 60
                    "SUV"   -> 100

carTotal :: Car -> [DayOfWeek] -> Int
carTotal _ [] = 0
carTotal car (x:xs)
    | x == Saturday || x == Sunday = dayPrice + carTotal car xs
    | otherwise                    = (dayPrice - (dayPrice `div` 10)) + (carTotal car xs)
   where
    dayPrice = carPrice car

subTotal :: Car -> [UTCTime] -> Int
subTotal _ [] = 0
subTotal car (x:xs) = carPrice car + subTotal car xs

discount :: Int -> Double -> Double
discount subT disc = 100-percentage
                  where
                     percentage = (disc/fromIntegral subT)*100

dayDiscount :: [DayOfWeek] -> Double
dayDiscount [] = 0
dayDiscount dates
    | days < 3   = 1
    | days >= 11 = 0.85
    | days >= 6  = 0.9
    | otherwise  = 0.95
   where
    days = length dates

afterDiscounts :: Car -> [DayOfWeek] -> Bool -> Double
afterDiscounts car days mem
    | mem = tot*0.95
    | otherwise = tot
   where
    tot = fromIntegral (carTotal car days) * (dayDiscount days)

insPerDay :: Car -> Double
insPerDay car = case carType car of
                    "small" -> 5
                    "sport" -> 7
                    "SUV"   -> 10

insurance :: [UTCTime] -> Car -> Int -> Double
insurance [] _ _ = 0
insurance dates car age
    | age < 25 = days*(insPerDay car)*1.25
    | otherwise = days*(insPerDay car)
   where
    days = fromIntegral (length dates)

roundTwo :: (Fractional a, RealFrac a1) => a1 -> a
roundTwo x = (fromInteger $ round $ x * (10^2)) / (10.0^^2)

toPriceInfo :: RentalInfo -> PriceInfo
toPriceInfo (RentalInfo {rentDates, car, membership, age}) = PriceInfo sub ins dis tot
    where
        sub             = subTotal car rentDates
        ins'            = insurance rentDates car age
        aft'            = afterDiscounts car dates membership
        dis'            = discount sub aft'
        tot'            = ins'+aft'
        dates           = map (dayOfWeek . utctDay) rentDates 
        [ins, dis, tot] = map roundTwo [ins', dis', tot']

is18 :: RentalInfo -> Maybe RentalInfo
is18 ri
    | age ri >= 18 = Just ri
    | otherwise = Nothing

validateRental :: ByteString -> Maybe RentalInfo
validateRental input = is18 =<< decode input :: Maybe RentalInfo 

calcTotal :: ByteString -> String
calcTotal input = case validateRental input of
                    Just rental -> show . encode $ toPriceInfo rental
                    Nothing     -> "The rental was not valid or renter is under 18"
