module ShowParserSpec where

import SpecHelper

data PersonRecord  = MkPersonRecord {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]    
} deriving (Show)

spec :: Spec
spec = 
  describe "Parsec" $ do
    context "context" $
      it "should be true" $
        True `shouldBe` True

main :: IO ()
main = hspec spec