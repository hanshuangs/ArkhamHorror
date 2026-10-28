module Arkham.Enemy.Cards.IreressSpec (spec) where

import Arkham.Ability.Types
import Arkham.Constants
import Arkham.Enemy.CardDefs.Fenris qualified as Enemies
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (InvestigatorActionsPerformed))
import Arkham.Placement
import Arkham.Projection (field)
import TestImport.New

spec :: Spec
spec = describe "Ireress" do
  it "retains its basic fight, evade, and engage actions" . gameTest $ \self -> do
    ireress <- createWeaknessEnemy self Enemies.ireress
    (map abilityIndex <$> ireress.abilities)
      `shouldMatchListM` [AbilityAttack, AbilityEvade, AbilityEngage, 1]

  for_ [AbilityAttack, AbilityEvade] \actionIndex -> do
    it ("grants action " <> show actionIndex <> " without spending an ordinary action")
      . gameTest
      $ \self -> do
        withProp @"resources" 2 self
        withProp @"combat" 5 self
        withProp @"agility" 5 self
        location <- testLocation
        self `moveTo` location
        ireress <- createWeaknessEnemy self Enemies.ireress
        -- Place the host directly; this test concerns its abilities, not swarm setup.
        run $ PlaceEnemy (toId ireress) (InThreatArea $ toId self)
        setChaosTokens [Zero]

        duringTurn self do
          withProp @"remainingActions" 0 self
          [grantAction] <- filter ((== 1) . abilityIndex) <$> self `getActionsFrom` ireress
          self `useAbility` grantAction
          chooseOptionMatching "choose the granted action" \case
            AbilityLabel {ability} -> abilityIndex ability == actionIndex
            _ -> False
          startSkillTest
          applyResults
          self.remainingActions `shouldReturn` 0
          self.resources `shouldReturn` 0
          field InvestigatorActionsPerformed (toId self)
            `shouldReturn` [[if actionIndex == AbilityAttack then #fight else #evade]]

  it "allows an investigator at its location to fight without being engaged with it"
    . gameTestWith Investigators.rolandBanks
    $ \self -> do
      withProp @"resources" 2 self
      withProp @"combat" 5 self
      location <- testLocation
      self `moveTo` location
      ireress <- createWeaknessEnemy self Enemies.ireress
      -- Avoid spawning its swarm: only the unengaged host's action choices
      -- are relevant to this case.
      overTest $ entitiesL . enemiesL . ix (toId ireress) %~ overAttrs (\a -> a {Enemy.enemyPlacement = AtLocation $ toId location})
      self.engagedEnemies `shouldReturn` []
      setChaosTokens [Zero]

      duringTurn self do
        [grantAction] <- filter ((== 1) . abilityIndex) <$> self `getActionsFrom` ireress
        self `useAbility` grantAction
        -- Evade is illegal while not engaged, so the sole legal fight resolves directly.
        startSkillTest
        applyResults
        self.remainingActions `shouldReturn` 3
        field InvestigatorActionsPerformed (toId self) `shouldReturn` [[#fight]]

  it "does not offer the ability when neither action is legal" . gameTest $ \self -> do
    withProp @"resources" 2 self
    location <- testLocation
    self `moveTo` location
    ireress <- createWeaknessEnemy self Enemies.ireress
    run $ PlaceEnemy (toId ireress) (InThreatArea $ toId self)
    run =<< gameModifier (TestSource mempty) (toTarget ireress) CannotBeAttacked
    run =<< gameModifier (TestSource mempty) (toTarget ireress) CannotBeEvaded

    duringTurn self do
      (filter ((== 1) . abilityIndex) <$> self `getActionsFrom` ireress) `shouldReturn` []
