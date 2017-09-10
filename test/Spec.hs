import Test.Hspec
import Data.Aeson

import Lib
import Parse

result = "\"{\\\"insuranceTotal\\\":2," ++
             "\\\"subtotal\\\":1," ++
             "\\\"discountPercentage\\\":3," ++
             "\\\"totalPayment\\\":4.5}\""
result2 = "\"{\\\"insuranceTotal\\\":28," ++
              "\\\"subtotal\\\":240," ++
              "\\\"discountPercentage\\\":9.75," ++
              "\\\"totalPayment\\\":244.6}\""
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
parsed2 = ([Just 6, Just 7,Just 1,Just 2],["sport","false","65"])
badParsed = ([Nothing,Just 1,Just 2],["sport","false","18"])
lowAge = ([Just 7,Just 1,Just 2],["sport","false","17"])
goodAge = ([Just 7,Just 1,Just 2],["sport","false","18"])

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
        it "rejects invalid gregorian dates" $ do
            toDateTime' ["2017", "2", "29"] `shouldBe` Nothing
        it "approves valid gregorian dates" $ do
            toDateTime' ["2020", "02", "29"] `shouldBe` Just 6
    describe "Validate RentalInfo and age" $ do
        it "can reject based on age" $ do
            validateRental lowAge `shouldBe` Nothing
        it "approves based on age" $ do
            validateRental goodAge `shouldBe` (Just $ RentalInfo [7,1,2] Sport False 18)
        it "rejects invalid rental dates" $ do
            validateRental badParsed `shouldBe` Nothing
        it "rejects invalid car" $ do
            validateRental ([Just 7,Just 1,Just 2],["spgort","false","18"]) `shouldBe` Nothing
        it "rejects invalid membership" $ do
            validateRental ([Just 7,Just 1,Just 2],["sport","fase","18"]) `shouldBe` Nothing
    describe "Calculate price and discounts" $ do
        it "can calculate correct price for a car weekend/weekdays" $ do
            carTotal Small [7,1,2] `shouldBe` 112
        it "can calculate correct discount for amount of days" $ do
            map dayDiscount [[1..3], [1..6], [1..11]] `shouldBe` [0.95, 0.90, 0.85]
        it "can calculate correct subTotal with member discount" $ do
            afterDiscounts SUV [6] True `shouldBe` 95
        it "can calculate correct subTotal without member discount" $ do
            afterDiscounts SUV [6] False `shouldBe` 100
        it "can calculate discount given ordinary and discounted price" $ do
            discount 5 2.0 `shouldBe` 60
        it "can calculate correct insurance cost for renter under 25" $ do
            insurance [1..5] Sport 24 `shouldBe` 43.75
        it "can calculate correct insurance cost for renter over 25" $ do
            insurance [1..5] Sport 25 `shouldBe` 35
    describe "Convert RentalInfo to PriceInfo" $ do
        it "can convert RentalInfo to PriceInfo" $ do
            toPriceInfo (RentalInfo [6,7,1,2] Sport False 65) `shouldBe` (PriceInfo 240 28 9.75 244.6)
        it "rounds floating point numbers to two decimals" $ do
            roundTwo (0.555555) `shouldBe` 0.56
    describe "Return JSON string of PriceInfo from parsed" $
        it "can convert parsed rental information to a JSON string" $ do
            calcTotal parsed2 `shouldBe` result2
