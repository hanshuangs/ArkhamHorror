module Arkham.Investigator.Cards.FenrisSpec (spec) where

import Arkham.Action.Additional
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Deck qualified as InvestigatorDeck
import Arkham.Skill.Cards qualified as Skills
import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map
import TestImport.New

-- Set the visible Wylder card without depending on the random round shuffle.
copyInvestigator :: Investigator -> CardDef -> TestAppT ()
copyInvestigator self def = do
  card <- genCard def
  updateInvestigator self \attrs ->
    attrs {investigatorDecks = Map.insert InvestigatorDeck.WylderDeck [card] (investigatorDecks attrs)}
  run Noop

reloadInvestigator :: Investigator -> TestAppT ()
reloadInvestigator self = do
  current <- getInvestigator self.id
  case fromJSON (toJSON current) of
    Error err -> expectationFailure $ "Could not reload Fenris: " <> err
    Success restored -> overTest $ entitiesL . investigatorsL . ix self.id .~ restored
  run Noop

spec :: Spec
spec = describe "Fenris" do
  it "copies printed skill values while retaining his own health and sanity"
    . gameTestWith Investigators.fenris
    $ \self -> do
      copyInvestigator self Investigators.finnEdwards
      self.willpower `shouldReturn` 1
      self.intellect `shouldReturn` 4
      self.combat `shouldReturn` 3
      self.agility `shouldReturn` 4
      current <- toAttrs <$> getInvestigator self.id
      investigatorHealth current `shouldBe` 7
      investigatorSanity current `shouldBe` 7

  it "copies Finn's additional evade action and sources it to Fenris"
    . gameTestWith Investigators.fenris
    $ \self -> do
      copyInvestigator self Investigators.finnEdwards
      assert
        $ self
        `hasModifier` GiveAdditionalAction (AdditionalAction "Finn Edwards" (toSource self) #evade)

  it "copies Calvin's passive skill modifiers"
    . gameTestWith Investigators.fenris
    $ \self -> do
      copyInvestigator self Investigators.calvinWright
      updateInvestigator self $ tokensL %~ (setTokens #damage 3 . setTokens #horror 2)
      run Noop
      self.willpower `shouldReturn` 2
      self.intellect `shouldReturn` 2
      self.combat `shouldReturn` 3
      self.agility `shouldReturn` 3

  it "retains Ursula's pending elder sign effect across messages and a save/load"
    . gameTestWith Investigators.fenris
    $ \self -> do
      (here, there) <- testConnectedLocations id id
      self `moveTo` here
      copyInvestigator self Investigators.ursulaDowns
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #agility 0
      reloadInvestigator self
      applyResults
      clickLabel "$label.moveToConnecting"
      chooseTarget there
      self.location `shouldReturn` Just (toId there)

  it "keeps Gloria's metadata separate when switching to Amanda"
    . gameTestWith Investigators.fenris
    $ \self -> do
      copyInvestigator self Investigators.gloriaGoldberg
      run $ UseThisAbility self.id (toSource self) 1
      current <- toAttrs <$> getInvestigator self.id
      investigatorMeta current `shouldBe` Null
      reloadInvestigator self
      copyInvestigator self Investigators.amandaSharpe
      card <- genPlayerCard Skills.guts
      other <- genMyCard self Skills.perception
      withProp @"hand" [other] self
      withProp @"deck" (Deck [card]) self
      run $ UseThisAbility self.id (toSource self) 1
      chooseTarget card
      field InvestigatorCardsUnderneath self.id `shouldReturn` [toCard card]

  it "loads legacy saves that have no Wylder state cache"
    . gameTestWith Investigators.fenris
    $ \self -> do
      case fromJSON @Investigator (toJSON $ toAttrs self) of
        Error err -> expectationFailure $ "Could not load legacy Fenris: " <> err
        Success restored -> do
          toCardCode restored `shouldBe` toCardCode Investigators.fenris
          toId restored `shouldBe` self.id
