module Arkham.Investigator.Cards.RobertScott (robertScott) where

import Arkham.Ability
import Arkham.Card
import Arkham.Cost
import Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Investigator.RobertScott
import Arkham.Matcher
import Arkham.Modifier (ModifierType (..))

newtype RobertScott = RobertScott InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

robertScott :: InvestigatorCard RobertScott
robertScott = investigator RobertScott Cards.robertScott
  $ Stats {health = 8, sanity = 6, willpower = 4, intellect = 1, combat = 4, agility = 3}

saveState :: RobertScottState -> InvestigatorAttrs -> InvestigatorAttrs
saveState state attrs = (attrs & setMeta state)
  & if null (availableExiledCards state)
      then deleteMetaKey "robertScottHasExiledCards"
      else setMetaKey "robertScottHasExiledCards" True

instance HasModifiersFor RobertScott where
  getModifiersFor (RobertScott attrs) = unless attrs.eliminated do
    modifySelect attrs (affectsColocated attrs.id <> not_ (be attrs)) [SharedResourcePool attrs.id]

instance HasAbilities RobertScott where
  getAbilities (RobertScott attrs) =
    [ withTooltip "$cards.tooltips.robertScott.exile" $ playerLimit PerRound
        $ restricted attrs 1 Self
        $ FastAbility (ExileFromCost $ InDiscardOf You <> basic (exilableLevels [1 .. 5]))
    ]
    <> [ withTooltip "$cards.tooltips.robertScott.experience" $ restricted attrs 2 (Self <> InVictoryDisplay (CardWithId cid) (atLeast 1))
           $ freeReaction $ oneOf [GameEnds #when, InvestigatorResigned #when You]
       | Just (cid, _) <- [(robertScottState attrs.meta).fireVictoryCard]
       , not (robertScottState attrs.meta).fireRewarded
       ]

instance HasChaosTokenValue RobertScott where
  getChaosTokenValue iid ElderSign (RobertScott attrs) | attrs `is` iid =
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RobertScott where
  runMessage msg i@(RobertScott attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ payment -> do
      for_ (chosenCardPayment payment) \cid -> do
        card <- getCard cid
        gainResources iid (attrs.ability 1) (fromMaybe 0 card.level)
      pure i
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let state = robertScottState attrs.meta
      for_ state.fireVictoryCard \(_, xp) -> gameModifier attrs iid (XPModifier "Put out the Fire" xp)
      pure $ RobertScott $ saveState (state {fireRewarded = True}) attrs
    Exile target -> do
      -- Remember the controller before an exiled asset/event leaves play. The
      -- owner can differ after Teamwork; "you have exiled" follows the actor.
      performed <- case target of
        CardIdTarget cid -> pure $ any ((== cid) . toCardId) (attrs.investigatorHand <> map toCard attrs.discard)
        AssetTarget aid -> selectAny (AssetWithId aid <> assetControlledBy attrs.id)
        EventTarget eid -> selectAny (EventWithId eid <> EventControlledBy (be attrs))
        _ -> pure False
      attrs' <- liftRunMessage msg attrs
      let state = robertScottState attrs.meta
      pure $ RobertScott $ if performed
        then saveState (state {pendingExiles = nub $ target : state.pendingExiles}) attrs'
        else attrs'
    Exiled target card
      | target `elem` (robertScottState attrs.meta).pendingExiles -> do
          attrs' <- liftRunMessage msg attrs
          let state = (robertScottState attrs.meta) {pendingExiles = filter (/= target) (robertScottState attrs.meta).pendingExiles}
          if any ((== toCardId card) . toCardId) state.exiledCards
            then pure $ RobertScott attrs'
            else do
              push $ RemoveCardFromDeckForCampaign (fromMaybe attrs.id card.owner) (toCardId card)
              pure $ RobertScott $ saveState (state {exiledCards = state.exiledCards <> [card]}) attrs'
    InvestigatorSpecific iid "robertScott.recover" value | attrs `is` iid -> do
      let state = robertScottState attrs.meta
      let cid = toResult value
      let state' = state {recoveredCards = nub $ cid : state.recoveredCards}
      pure $ RobertScott $ saveState state' attrs
    InvestigatorSpecific iid "robertScott.putOutTheFire" value | attrs `is` iid -> do
      let state = robertScottState attrs.meta
      pure $ RobertScott $ saveState (state {fireVictoryCard = Just $ toResult value}) attrs
    ElderSignEffect iid | attrs `is` iid -> do
      let state = robertScottState attrs.meta
      pure $ RobertScott $ saveState (state {elderSignPending = True}) attrs
    SkillTestEnds _ iid _ | attrs `is` iid, (robertScottState attrs.meta).elderSignPending -> do
      chooseOne iid
        [ Label "$label.cards.robertScott.gainResource" [TakeResources iid 1 (toSource ElderSign) False]
        , Label "$label.cards.robertScott.skipResource" []
        ]
      let state = robertScottState attrs.meta
      pure $ RobertScott $ saveState (state {elderSignPending = False}) attrs
    ForInvestigators _ ResetGame -> do
      attrs' <- liftRunMessage msg attrs
      pure $ RobertScott $ saveState emptyRobertScottState attrs'
    _ -> RobertScott <$> liftRunMessage msg attrs
