module Arkham.Event.Events.JetStrike (jetStrike) where

import Arkham.Card
import Arkham.Cost (chosenCardPayment)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.RobertScott
import Arkham.Modifier

newtype JetStrike = JetStrike EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jetStrike :: EventCard JetStrike
jetStrike = event JetStrike Cards.jetStrike

instance RunMessage JetStrike where
  runMessage msg e@(JetStrike attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      state <- getRobertScottState iid
      bonus <- case chosenCardPayment attrs.payment of
        Nothing -> pure 0
        Just cid -> fromMaybe 0 . (.level) <$> getCard cid
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #combat bonus, DamageDealt $ min 3 $ length state.exiledCards]
      chooseFightEnemy sid iid attrs
      pure e
    _ -> JetStrike <$> liftRunMessage msg attrs
