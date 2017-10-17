module ShowParserSpec where

import SpecHelper

data PersonRecord  = MkPersonRecord {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]    
} deriving (Show)

data Address = MkAddress {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 = MkPersonRecord 
  "Wim Vanderbauwhede" 
  (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
  557188
  [Green, Red]

rec2 = MkPersonRecord 
  "Jeremy Singer" 
  (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
  42
  [Blue, Yellow]

spec :: Spec
spec = 
  describe "Parsec" $ do
    context "show PersonRecord Wim" $
      it "should equal to string representation" $
        show rec1 `shouldBe` "MkPersonRecord {name = \"Wim Vanderbauwhede\", address = MkAddress {line1 = \"School of Computing Science\", number = 17, street = \"Lilybank Gdns\", town = \"Glasgow\", postcode = \"G12 8QQ\"}, id = 557188, labels = [Green,Red]}"

    context "show PersonRecord Jeremy" $
      it "should equal to string representation" $
        show rec2 `shouldBe` "MkPersonRecord {name = \"Jeremy Singer\", address = MkAddress {line1 = \"School of Computing Science\", number = 17, street = \"Lilybank Gdns\", town = \"Glasgow\", postcode = \"G12 8QQ\"}, id = 42, labels = [Blue,Yellow]}"

main :: IO ()
main = hspec spec