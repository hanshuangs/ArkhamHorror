module Arkham.Asset.Assets.WyldsideSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.PlayerCard (allPlayerCards)
import Data.Map.Strict qualified as Map
import TestImport qualified as TI
import TestImport.New

spec :: Spec
spec = describe "Wyldside" do
  it "registers as a player card so deck imports accept its card code" do
    Map.lookup "99101" Assets.allPlayerAssetCards `TI.shouldBe` Just Assets.wyldside
    Map.lookup "99101" allPlayerCards `TI.shouldBe` Just Assets.wyldside
    Map.member "99101" Assets.allEncounterAssetCards `TI.shouldBe` False

  it "spends an action and exhausts to draw two cards" . gameTest $ \self -> do
    cards <- testPlayerCards 3
    withProp @"deck" (Deck cards) self
    location <- testLocation
    self `moveTo` location
    wyldside <- self `putAssetIntoPlay` Assets.wyldside

    duringTurn self do
      [drawTwo] <- self `getActionsFrom` wyldside
      self `useAbility` drawTwo
      self.remainingActions `shouldReturn` 2
      self.hand `shouldMatchListM` map toCard (take 2 cards)
      wyldside.exhausted `shouldReturn` True
      self `getActionsFrom` wyldside `shouldReturn` []
