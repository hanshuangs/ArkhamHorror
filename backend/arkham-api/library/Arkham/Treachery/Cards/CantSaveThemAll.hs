module Arkham.Treachery.Cards.CantSaveThemAll (cantSaveThemAll) where

import Arkham.Ability
import Arkham.Investigator.RobertScott (exilableLevels)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.RobertScott qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CantSaveThemAll = CantSaveThemAll TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cantSaveThemAll :: TreacheryCard CantSaveThemAll
cantSaveThemAll = treachery CantSaveThemAll Cards.cantSaveThemAll

instance HasAbilities CantSaveThemAll where
  getAbilities (CantSaveThemAll attrs) =
    [ restricted attrs 1 (InThreatAreaOf You)
        $ forced $ oneOf
          [ InvestigatorTakeDamage #after (affectsOthers $ colocatedWithMatch You <> not_ You) AnySource
          , InvestigatorTakeHorror #after (affectsOthers $ colocatedWithMatch You <> not_ You) AnySource
          ]
    , restricted attrs 2 (InThreatAreaOf You)
        $ ActionAbility [] Nothing
        $ OrCost [ActionCost 3, ActionCost 1 <> ExileFromCost (InHandOf ForPlay You <> basic (exilableLevels [0 .. 5]))]
    ]

instance RunMessage CantSaveThemAll where
  runMessage msg t@(CantSaveThemAll attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> placeInThreatArea attrs iid >> pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> assignHorror iid (attrs.ability 1) 1 >> pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> toDiscardBy iid (attrs.ability 2) attrs >> pure t
    _ -> CantSaveThemAll <$> liftRunMessage msg attrs
