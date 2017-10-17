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

spec :: Spec
spec = 
  describe "Parsec" $ do
    context "show PersonRecord 1" $
      it "should equal to string representation" $
        show rec1 `shouldBe` "MkPersonRecord {name = \"Wim Vanderbauwhede\", address = MkAddress {line1 = \"School of Computing Science\", number = 17, street = \"Lilybank Gdns\", town = \"Glasgow\", postcode = \"G12 8QQ\"}, id = 557188, labels = [Green,Red]}"

main :: IO ()
main = hspec spec