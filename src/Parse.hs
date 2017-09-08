module Parse
    ( parseInput
    ) where

import Data.List.Split
import Data.Maybe
import Text.Read
import Data.Time (fromGregorianValid)
import Data.Time.Calendar.WeekDate (toWeekDate)

chars = ['\"', '{', '}', ' ']

removeChars :: String -> String
removeChars [] = []
removeChars (x:xs)
    |x `elem` chars = removeChars xs
    |otherwise      = x : removeChars xs

parseInput :: String -> ([Maybe Int], [String])
parseInput input = parse . getDates $ removeChars input

parse :: (String, String) -> ([Maybe Int], [String])
parse (dates, rest) = (wds, rst)
                     where
                        wds = map toWeekDay (splitOn "," dates)
                        rst = map getSecondVal (drop 2 (splitOn "," rest))

getSecondVal :: String -> String
getSecondVal [] = []
getSecondVal (x:xs)
    |x == ':'  = xs
    |otherwise = getSecondVal xs

getDates, getDates' :: String -> (String, String)
getDates [] = ([],[])
getDates (x:xs)
    |x == '['  = getDates' xs
    |otherwise = getDates xs

getDates' [] = ([],[])
getDates' (x:xs) 
    |x == ']'  = ([],xs)
    |otherwise = (x:ys,zs)
   where (ys, zs) = getDates' xs

toWeekDay :: String -> Maybe Int
toWeekDay date = toDateTime $ splitOn "T" date

toDateTime, toDateTime' :: [String] -> Maybe Int
toDateTime [date, _] = case (splitOn "-" date) of
                            [y,m,d] -> toDateTime' [y,m,d]
                            _       -> Nothing
toDateTime _ = Nothing

toDateTime' [y,m,d] = case (yy,mm,dd) of
                            (Just ye, Just mo, Just day) -> validDate ye mo day
                            _                            -> Nothing
                        where
                            yy = readMaybe y :: Maybe Int
                            mm = readMaybe m :: Maybe Int
                            dd = readMaybe d :: Maybe Int
toDateTime' _ = Nothing

validDate :: Int -> Int -> Int -> Maybe Int
validDate y m d = case rentalDay of
                    Just day -> Just $ validDate' (toWeekDate day)
                    Nothing  -> Nothing
                where
                    rentalDay = fromGregorianValid (fromIntegral y) m d
                    validDate' (_,_,wd) = wd
