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

spec :: Spec
spec = 
  describe "Parsec" $ do
    context "context" $
      it "should be true" $
        True `shouldBe` True
      --it "should be false" $
        --False `shouldBe` False

main :: IO ()
main = hspec spec