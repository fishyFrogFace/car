import Test.Hspec
import Data.Aeson

import Lib
import Parse

result = "\"{\\\"insuranceTotal\\\":2," ++
             "\\\"subtotal\\\":1," ++
             "\\\"discountPercentage\\\":3," ++
             "\\\"totalPayment\\\":4.5}\""
input = "\"{\"rentDates\":" ++
                "[\"2017-11-19T05:00:00.000Z\"," ++
                "\"2017-11-20T05:00:00.000Z\"," ++
                "\"2017-11-21T05:00:00.000Z\"]," ++
        "\"car\":{" ++
                "\"model\":\"Cherato\"," ++
                "\"type\":\"sport\"}," ++
        "\"membership\":false," ++
        "\"age\":24}\""
parsed = ([Just 7,Just 1,Just 2],["sport","false","24"])

main :: IO ()
main = hspec $ do
    describe "PriceInfo to JSON for output" $ do
        it "can convert resulting values to JSON" $ do
            show (encode (PriceInfo 1 2 3 4.5)) `shouldBe` result 
    describe "Price per day for car type" $ do
        it "returns correct price for car types" $ do
            map carPrice [Small, Sport, SUV] `shouldBe` [40, 60, 100]
    describe "From JSON for processing" $ do
        it "can parse JSON" $ do
            parseInput input `shouldBe` parsed
        it "can create RentalInfo from parsed JSON" $ do
            toRentalInfo parsed `shouldBe` (Just $ RentalInfo [7,1,2] Sport False 24)
