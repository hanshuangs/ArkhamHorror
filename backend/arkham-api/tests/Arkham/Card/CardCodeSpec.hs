module Arkham.Card.CardCodeSpec (spec) where

import Arkham.Card.CardCode
import Data.Aeson (eitherDecode, encode)
import Data.Map.Strict qualified as Map
import Test.Hspec
import Prelude (Either (Right), Int, String)

spec :: Spec
spec = describe "CardCode equality" do
  it "pairs the two sides of a card" do
    CardCode "01234a" `shouldBe` CardCode "01234b"

  it "pairs the stranger's exception codes with their b-side" do
    CardCode "03047a" `shouldBe` CardCode "03047ab"

  it "does not pair a bare code with its b-side" do
    CardCode "11550" `shouldNotBe` CardCode "11550b"

  it "keeps distinct printings apart" do
    CardCode "10510a" `shouldNotBe` CardCode "10510b"

  -- The four children of Public School 187 are cards 63a-63d, each double
  -- sided, so their codes carry two trailing letters: the card letter and then
  -- the side. Only the last letter is a side designator.
  it "pairs a two-letter tail with its own other side" do
    CardCode ":dark-matter:063da" `shouldBe` CardCode ":dark-matter:063db"
    CardCode ":dark-matter:063ca" `shouldBe` CardCode ":dark-matter:063cb"

  it "does not cross-match different cards sharing a base" do
    CardCode ":dark-matter:063da" `shouldNotBe` CardCode ":dark-matter:063cb"
    CardCode ":dark-matter:063aa" `shouldNotBe` CardCode ":dark-matter:063bb"
    CardCode ":dark-matter:063da" `shouldNotBe` CardCode ":dark-matter:063aa"

  it "loads legacy Fenris card values using the native arkham.build codes" do
    eitherDecode "[\"99100\",\"c99100\",\"99101\",\"c99101\",\"99102\",\"c99102\"]"
      `shouldBe` (Right ["net12", "net12", "twoa10002733", "twoa10002733", "twoa10002734", "twoa10002734"] :: Either String [CardCode])

  it "loads legacy Fenris object keys using the same native codes as card values" do
    eitherDecode "{\"c99100\":1,\"99101\":2,\"c99102\":3}"
      `shouldBe` (Right (Map.fromList [("net12", 1), ("twoa10002733", 2), ("twoa10002734", 3)]) :: Either String (Map.Map CardCode Int))

  it "round trips native Fenris codes without changing official or other custom codes" do
    let codes = ["net12", "twoa10002733", "twoa10002734", "01001", "*custom0"] :: [CardCode]
    eitherDecode (encode codes) `shouldBe` Right codes
