module Arkham.Event.Events.PutOutTheFire (putOutTheFire) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.RobertScott

newtype PutOutTheFire = PutOutTheFire EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

putOutTheFire :: EventCard PutOutTheFire
putOutTheFire = event PutOutTheFire Cards.putOutTheFire

instance RunMessage PutOutTheFire where
  runMessage msg e@(PutOutTheFire attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- availableExiledCards <$> getRobertScottState iid
      chooseOne iid $ targetLabels cards \card ->
        [ AddToVictory (Just iid) (toTarget card)
        , InvestigatorSpecific iid "robertScott.recover" (toJSON $ toCardId card)
        , InvestigatorSpecific iid "robertScott.putOutTheFire" (toJSON (toCardId attrs, fromMaybe 0 card.level))
        , AddToVictory (Just iid) (toTarget attrs)
        ]
      pure e
    _ -> PutOutTheFire <$> liftRunMessage msg attrs
