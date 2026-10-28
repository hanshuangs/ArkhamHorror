module Arkham.Asset.Assets.WyldsideSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Wyldside" do
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
