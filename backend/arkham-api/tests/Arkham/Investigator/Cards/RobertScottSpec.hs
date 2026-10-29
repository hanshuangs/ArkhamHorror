module Arkham.Investigator.Cards.RobertScottSpec (spec) where

import Arkham.Ability.Types (abilityIndex)
import Arkham.Event qualified as RuntimeEvents
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Xp (getXpWithBonus)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator qualified as RuntimeInvestigators
import Arkham.Matcher.Event (EventMatcher (EventWithCardId, EventWithId))
import Arkham.Investigator.RobertScott
import Arkham.Matcher.Skill (SkillMatcher (SkillWithId))
import Arkham.Placement (Placement (Limbo))
import Arkham.PlayerCard (allPlayerCards)
import Arkham.Skill.Cards qualified as Skills
import Arkham.Treachery.CardDefs.RobertScott qualified as Treacheries
import Arkham.Treachery qualified as RuntimeTreacheries
import Data.Map.Strict qualified as Map
import TestImport qualified as TI
import TestImport.New

spec :: Spec
spec = describe "Robert Scott" do
  it "registers all three signature cards under their native arkham.build IDs" do
    Map.member "a5a24167-f20e-40b9-92ed-551374516701" RuntimeInvestigators.allInvestigators `TI.shouldBe` True
    Map.member "50571516-0d4a-46da-b8c3-6ba26b2baaec" RuntimeEvents.allEvents `TI.shouldBe` True
    Map.member "6c392683-6c14-4c45-a5d6-25d58d433433" RuntimeEvents.allEvents `TI.shouldBe` True
    Map.member "675f833b-dd06-4784-b785-c9f3762fbef2" RuntimeTreacheries.allTreacheries `TI.shouldBe` True
    Map.lookup "50571516-0d4a-46da-b8c3-6ba26b2baaec" allPlayerCards `TI.shouldBe` Just Events.jetStrike
    Map.lookup "6c392683-6c14-4c45-a5d6-25d58d433433" allPlayerCards `TI.shouldBe` Just Events.putOutTheFire
    Map.lookup "675f833b-dd06-4784-b785-c9f3762fbef2" allPlayerCards `TI.shouldBe` Just Treacheries.cantSaveThemAll

  it "exiles a card from his discard as a cost, gains its level, and is limited per round"
    . gameTestWith Investigators.robertScott $ \self -> do
      PlayerCard card <- genMyCard self Events.aTestOfWill1
      withProp @"discard" [card] self
      withProp @"resources" 0 self
      duringRound do
        [ability] <- self `getActionsFrom` self
        self `useAbility` ability
        chooseTarget card
        self.discard `shouldReturn` []
        self.resources `shouldReturn` 1
        self `getActionsFrom` self `shouldReturn` []
      state <- getRobertScottState self.id
      state.exiledCards `shouldBe` [toCard card]

  it "cannot exile a level-zero or signature card using his free ability"
    . gameTestWith Investigators.robertScott $ \self -> do
      PlayerCard zero <- genMyCard self Events.emergencyCache
      PlayerCard signature <- genMyCard self Events.jetStrike
      withProp @"discard" [zero, signature] self
      self `getActionsFrom` self `shouldReturn` []

  it "shares only his own resources with colocated teammates and stops sharing after they move"
    . gameTestWith Investigators.robertScott $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      (here, there) <- testConnectedLocations id id
      self `moveTo` here
      other `moveTo` here
      withProp @"resources" 3 self
      withProp @"resources" 1 other
      getSpendableResources other.id `shouldReturn` 4
      getSpendableResources self.id `shouldReturn` 3
      other `moveTo` there
      getSpendableResources other.id `shouldReturn` 1

  it "deducts a teammate's payment from Robert's pool"
    . gameTestWith Investigators.robertScott $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      here <- testLocation
      self `moveTo` here
      other `moveTo` here
      withProp @"resources" 2 self
      withProp @"resources" 0 other
      run $ SpendResources other.id 2
      -- Both available resource choices belong to Robert, so no split is needed.
      self.resources `shouldReturn` 0
      other.resources `shouldReturn` 0

  it "does not borrow Robert's resources when a teammate loses resources"
    . gameTestWith Investigators.robertScott $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      here <- testLocation
      self `moveTo` here
      other `moveTo` here
      withProp @"resources" 3 self
      withProp @"resources" 0 other
      run $ LoseResources other.id GameSource 2
      self.resources `shouldReturn` 3
      other.resources `shouldReturn` 0

  it "keeps cumulative exile history when a card is put into the victory display"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Events.aTestOfWill1
      withProp @"hand" [card] self
      run $ Exile (toTarget card)
      run $ InvestigatorSpecific self.id "robertScott.recover" (toJSON $ toCardId card)
      state <- getRobertScottState self.id
      length state.exiledCards `shouldBe` 1
      availableExiledCards state `shouldBe` []
      -- Saved metadata retains both the cumulative count and recovered status.
      robertScottState (toJSON state) `shouldBe` state

  it "offers the elder-sign resource after the test ends"
    . gameTestWith Investigators.robertScott $ \self -> do
      withProp @"resources" 0 self
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #agility 0
      self.resources `shouldReturn` 0
      applyResults
      clickLabel "$label.cards.robertScott.gainResource"
      self.resources `shouldReturn` 1

  it "records a committed skill that is exiled from play"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Skills.strokeOfLuck2
      sid <- getRandom
      run $ CreateSkill sid card self.id Limbo
      run $ Exile (SkillTarget sid)
      select (SkillWithId sid) `shouldReturn` []
      state <- getRobertScottState self.id
      state.exiledCards `shouldBe` [card]
      availableExiledCards state `shouldBe` [card]

  it "records an event that is exiled while resolving in limbo"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Events.aTestOfWill1
      run $ CreateEventAt self.id card Limbo
      eid <- selectJust $ EventWithCardId (toCardId card)
      run $ Exile (EventTarget eid)
      select (EventWithId eid) `shouldReturn` []
      state <- getRobertScottState self.id
      state.exiledCards `shouldBe` [card]
      availableExiledCards state `shouldBe` [card]

  it "Jet Strike counts earlier exiles even after recovery, with a maximum of three bonus damage"
    . gameTestWith Investigators.robertScott $ \self -> do
      cards <- replicateM 4 (genMyCard self Events.emergencyCache)
      withProp @"hand" cards self
      for_ cards $ run . Exile . toTarget
      for_ (listToMaybe cards) \card -> run $ InvestigatorSpecific self.id "robertScott.recover" (toJSON $ toCardId card)
      here <- testLocation
      enemy <- testEnemy & prop @"fight" 0 & prop @"health" 8
      self `moveTo` here
      enemy `spawnAt` here
      setChaosTokens [Zero]
      self `playEvent` Events.jetStrike
      chooseTarget enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 4

  it "Jet Strike's optional exile is paid before the fight and adds that card's level to combat"
    . gameTestWith Investigators.robertScott $ \self -> do
      jet <- genMyCard self Events.jetStrike
      boost <- genMyCard self Events.aTestOfWill2
      withProp @"hand" [jet, boost] self
      withProp @"resources" 2 self
      here <- testLocation
      enemy <- testEnemy & prop @"fight" 6 & prop @"health" 8
      self `moveTo` here
      enemy `spawnAt` here
      setChaosTokens [Zero]
      duringTurn self do
        self `playCard` jet
        chooseOptionMatching "pay the optional exile" \case
          CostLabel (ExileFromCost _) _ -> True
          _ -> False
        chooseTarget boost
        chooseTarget enemy
        startSkillTest
        applyResults
        enemy.damage `shouldReturn` 2
        self.hand `shouldReturn` []

  it "Can't Save Them All costs three actions without an exile"
    . gameTestWith Investigators.robertScott $ \self -> do
      weakness <- self `putTreacheryIntoPlay` Treacheries.cantSaveThemAll
      duringTurn self do
        [ability] <- self `getActionsFrom` weakness
        self `useAbility` ability
        self.remainingActions `shouldReturn` 0

  it "Can't Save Them All causes one horror when a colocated teammate takes several damage"
    . gameTestWith Investigators.robertScott $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      here <- testLocation
      self `moveTo` here
      other `moveTo` here
      self `drawsCard` Treacheries.cantSaveThemAll
      run $ assignDamage other.id (TestSource mempty) 3
      applyAllDamage
      useForcedAbility
      applyAllHorror
      self.horror `shouldReturn` 1
      other.damage `shouldReturn` 3

  it "Can't Save Them All does not trigger for Robert or for a teammate at another location"
    . gameTestWith Investigators.robertScott $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      (here, there) <- testConnectedLocations id id
      self `moveTo` here
      other `moveTo` there
      self `drawsCard` Treacheries.cantSaveThemAll
      run $ assignDamage self.id (TestSource mempty) 1
      applyAllDamage
      run $ assignDamage other.id (TestSource mempty) 1
      applyAllDamage
      self.horror `shouldReturn` 0

  it "Can't Save Them All can exile a level-zero hand card to cost only one action"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Events.emergencyCache
      withProp @"hand" [card] self
      weakness <- self `putTreacheryIntoPlay` Treacheries.cantSaveThemAll
      duringTurn self do
        abilities <- self `getActionsFrom` weakness
        [ability] <- pure $ filter ((== 3) . abilityIndex) abilities
        self `useAbility` ability
        chooseTarget card
        self.remainingActions `shouldReturn` 2
        self.hand `shouldReturn` []

  it "adds Put out the Fire's selected level to experience only once"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Events.putOutTheFire
      run $ AddToVictory (Just self.id) (toTarget card)
      run $ InvestigatorSpecific self.id "robertScott.putOutTheFire" (toJSON (toCardId card, 3 :: Int))
      endGame
      useReaction
      getXpWithBonus 0 `shouldReturn` [(self.id, 3)]
      self `getActionsFrom` self `shouldReturn` []

  it "Put out the Fire needs an available exile and can only be played as the first action"
    . gameTestWith Investigators.robertScott $ \self -> do
      fire <- genMyCard self Events.putOutTheFire
      card <- genMyCard self Events.aTestOfWill1
      here <- testLocation
      self `moveTo` here
      withProp @"hand" [fire, card] self
      duringTurn self do
        self.playableCards `shouldSatisfyM` notElem fire
        run $ Exile (toTarget card)
        self.playableCards `shouldSatisfyM` elem fire
        takeResource self
        self.playableCards `shouldSatisfyM` notElem fire

  it "Put out the Fire moves both cards into the victory display and preserves the exile count"
    . gameTestWith Investigators.robertScott $ \self -> do
      card <- genMyCard self Events.aTestOfWill1
      withProp @"hand" [card] self
      run $ Exile (toTarget card)
      self `playEvent` Events.putOutTheFire
      chooseTarget card
      victory <- getVictoryDisplay
      length victory `shouldBe` 2
      map toCardId victory `shouldSatisfy` elem (toCardId card)
      state <- getRobertScottState self.id
      length state.exiledCards `shouldBe` 1
      availableExiledCards state `shouldBe` []
      endGame
      useReaction
      getXpWithBonus 0 `shouldReturn` [(self.id, 1)]
