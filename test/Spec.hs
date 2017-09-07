import Test.Hspec
import Data.Aeson
import Lib

result = "\"{\\\"insuranceTotal\\\":2," ++
             "\\\"subtotal\\\":1," ++
             "\\\"discountPercentage\\\":3," ++
             "\\\"totalPayment\\\":4.5}\""

main :: IO ()
main = hspec $ do
    describe "PriceInfo to JSON" $ do
        it "can convert resulting values to json" $ do
             show (encode (PriceInfo 1 2 3 4.5)) `shouldBe` result 
    describe "Price per day for car type" $ do
        it "returns correct price for car types" $ do
            map carPrice [Small, Sport, SUV] `shouldBe` [40, 60, 100]
