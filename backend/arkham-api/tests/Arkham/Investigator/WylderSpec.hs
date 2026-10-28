module Arkham.Investigator.WylderSpec (spec) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Wylder
import Arkham.Prelude
import Data.Set qualified as Set
import Test.Hspec

spec :: Spec
spec = describe "Fenris' Wylder investigator choices" do
  it "accepts four investigators with different classes" do
    validWylderSelection ["01001", "01002", "01003", "01004"] `shouldBe` True

  it "requires exactly four choices and rejects unknown or repeated identities" do
    validWylderSelection ["01001", "01002", "01003"] `shouldBe` False
    validWylderSelection ["01001", "01002", "01003", "01004", "01005"] `shouldBe` False
    validWylderSelection ["01001", "01002", "01003", "missing"] `shouldBe` False
    validWylderSelection ["01001", "01001", "01003", "01004"] `shouldBe` False

  it "accepts promo card codes but offers each printing only once" do
    validWylderSelection ["98004", "01002", "01003", "01004"] `shouldBe` True
    let codes = map canonicalCardCode (wylderCandidates [])
    codes `shouldBe` ordNub codes

  it "rejects investigators sharing a class" do
    validWylderSelection ["01001", "02001", "01003", "01004"] `shouldBe` False
    map cdCardCode (wylderCandidates ["01001"]) `shouldNotContain` ["02001"]

  it "does not treat neutral as a class or allow a second version of the same person" do
    validWylderSelection ["03006", "09018", "01001", "01002"] `shouldBe` True
    validWylderSelection ["03006", "90087", "01001", "01002"] `shouldBe` False
    validWylderSelection ["11007", "11008", "01001", "01003"] `shouldBe` False
    map cdCardCode (wylderCandidates ["03006"]) `shouldNotContain` ["90087"]

  it "counts all classes of a multiclass investigator" do
    let multiclass = Cards.rolandBanks {cdClassSymbols = Set.fromList [Guardian, Seeker]}
    wylderClasses multiclass `shouldBe` Set.fromList [Guardian, Seeker]
    compatibleWylderInvestigator [multiclass] Cards.daisyWalker `shouldBe` False
    compatibleWylderInvestigator [multiclass] Cards.skidsOToole `shouldBe` True

  it "excludes Fenris, nonstarting forms, and campaign-only investigators" do
    let unavailable :: [CardCode]
        unavailable = ["net12", "10016a", "10016b", "11068b", "04244", "05046", "10661"]
    for_ unavailable \code -> do
      map cdCardCode (wylderCandidates []) `shouldNotContain` [code]
      validWylderSelection [code, "01001", "01002", "01003"] `shouldBe` False

  it "has no successors after four choices or an invalid partial selection" do
    wylderCandidates ["01001", "01002", "01003", "01004"] `shouldBe` []
    wylderCandidates ["missing"] `shouldBe` []
    wylderCandidates ["01001", "02001"] `shouldBe` []
