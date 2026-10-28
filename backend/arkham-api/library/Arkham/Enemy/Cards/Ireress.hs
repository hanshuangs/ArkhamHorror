module Arkham.Enemy.Cards.Ireress (ireress, Ireress (..)) where

import Arkham.Ability
import Arkham.Cost (Cost (..))
import Arkham.Enemy.CardDefs.Fenris qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (ActionCostModifier))
import Arkham.Trait (Trait (Drifter))

newtype Ireress = Ireress EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ireress :: EnemyCard Ireress
ireress =
  enemyWith
    Ireress
    Cards.ireress
    (\a -> a & preyL .~ OnlyPrey (Prey (InvestigatorWithTrait Drifter)))

instance HasAbilities Ireress where
  getAbilities (Ireress attrs) =
    extend
      attrs
      [ restrictedAbility
          attrs
          1
          ( not_ DuringAction
              <> exists (ireressActions attrs <> PerformableAbility [ActionCostModifier (-1)])
          )
          $ FastAbility (ResourceCost 2)
      ]

ireressActions :: EnemyAttrs -> AbilityMatcher
ireressActions attrs =
  BasicAbility <> AbilityOnEnemy (be attrs) <> oneOf [#fight, #evade]

instance RunMessage Ireress where
  runMessage msg e@(Ireress attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Resolve the granted action through its normal ability pipeline so it
      -- retains action windows and additional costs, without spending the base
      -- action. This also filters out unavailable fight/evade choices.
      abilities <-
        selectMap (`decrease_` 1)
          $ performableAbilityWithoutActionBy iid (ireressActions attrs)
      chooseOrRunOneM iid $ for_ abilities (abilityLabeled_ iid)
      pure e
    _ -> Ireress <$> liftRunMessage msg attrs
