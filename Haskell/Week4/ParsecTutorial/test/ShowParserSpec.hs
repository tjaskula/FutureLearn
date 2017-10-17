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
        parseShow (show rec1) `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<record name=\"MkPersonRecord\"><elt key=\"name\">\"Wim Vanderbauwhede\"</elt>\n<elt key=\"address\"><record name=\"MkAddress\"><elt key=\"line1\">\"School of Computing Science\"</elt>\n<elt key=\"number\">17</elt>\n<elt key=\"street\">\"Lilybank Gdns\"</elt>\n<elt key=\"town\">\"Glasgow\"</elt>\n<elt key=\"postcode\">\"G12 8QQ\"</elt></record></elt>\n<elt key=\"id\">557188</elt>\n<elt key=\"labels\"><list><list-elt><adt>Green</adt></list-elt>\n<list-elt><adt>Red</adt></list-elt></list></elt></record>"

    context "show PersonRecord Jeremy" $
      it "should equal to string representation" $
        parseShow (show rec2) `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<record name=\"MkPersonRecord\"><elt key=\"name\">\"Jeremy Singer\"</elt>\n<elt key=\"address\"><record name=\"MkAddress\"><elt key=\"line1\">\"School of Computing Science\"</elt>\n<elt key=\"number\">17</elt>\n<elt key=\"street\">\"Lilybank Gdns\"</elt>\n<elt key=\"town\">\"Glasgow\"</elt>\n<elt key=\"postcode\">\"G12 8QQ\"</elt></record></elt>\n<elt key=\"id\">42</elt>\n<elt key=\"labels\"><list><list-elt><adt>Blue</adt></list-elt>\n<list-elt><adt>Yellow</adt></list-elt></list></elt></record>"

main :: IO ()
main = hspec spec