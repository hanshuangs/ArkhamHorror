module Arkham.Asset.Assets.Wyldside (wyldside, Wyldside (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Cost (Cost (..))

newtype Wyldside = Wyldside AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wyldside :: AssetCard Wyldside
wyldside = asset Wyldside Cards.wyldside

instance HasAbilities Wyldside where
  getAbilities (Wyldside a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage Wyldside where
  runMessage msg a@(Wyldside attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawCards iid (attrs.ability 1) 2
      pure a
    _ -> Wyldside <$> liftRunMessage msg attrs
