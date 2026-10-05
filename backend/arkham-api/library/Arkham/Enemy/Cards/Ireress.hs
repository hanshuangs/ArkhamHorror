module Arkham.Enemy.Cards.Ireress (ireress, Ireress (..)) where

import Arkham.Ability
import Arkham.Cost (Cost (..))
import Arkham.Enemy.CardDefs.Fenris qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Evade (mkChooseEvade)
import Arkham.Fight (mkFightEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
    [ restrictedAbility attrs 1 (exists $ be attrs <> EnemyIsEngagedWith You)
        $ FastAbility
        $ ResourceCost 2
    ]

instance RunMessage Ireress where
  runMessage msg e@(Ireress attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        labeledI "fight" do
          cf <- mkFightEnemy sid iid (attrs.ability 1) (toId attrs)
          push $ FightEnemy (toId attrs) cf
        labeledI "evade" do
          ce <- mkChooseEvade sid iid (attrs.ability 1)
          push $ ChooseEvadeEnemy ce {chooseEvadeEnemyMatcher = EnemyWithId (toId attrs)}
      pure e
    _ -> Ireress <$> liftRunMessage msg attrs
