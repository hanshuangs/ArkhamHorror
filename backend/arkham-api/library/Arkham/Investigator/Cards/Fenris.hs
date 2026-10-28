module Arkham.Investigator.Cards.Fenris (fenris) where

import Arkham.Ability
import Arkham.Card
import Arkham.Card.Settings
import Arkham.Helpers.Investigator (getAttrStats, getCardAttachments)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.I18n
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Deck
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Wylder (validWylderSelection, wylderCandidates)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import {-# SOURCE #-} Arkham.Investigator
import Data.Map.Strict qualified as Map

-- Keep each impersonated investigator's complete state, including any metadata
-- carried outside InvestigatorAttrs by its concrete type. The prefixed field
-- deliberately lives outside attrs.meta, which belongs to Fenris himself.
newtype Metadata = Metadata {fenrisWylderStates :: Map CardCode Value}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON Metadata where
  parseJSON = withObject "Fenris metadata" $ \o ->
    Metadata <$> o .:? "fenrisWylderStates" .!= mempty

newtype Fenris = Fenris (InvestigatorAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator Fenris where
  investigatorFromAttrs = Fenris . (`with` Metadata mempty)

fenris :: InvestigatorCard Fenris
fenris =
  investigator (Fenris . (`with` Metadata mempty)) Cards.fenris
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 2, combat = 2, agility = 2}

-- | The Wylder deck is the four investigators chosen at deck creation.
wylderDeck :: InvestigatorAttrs -> [Card]
wylderDeck = Map.findWithDefault [] WylderDeck . investigatorDecks

wylderTop :: InvestigatorAttrs -> Maybe Card
wylderTop = listToMaybe . wylderDeck

{- | Reproduce the top Wylder card's investigator behaviour with Fenris' shared
game state and id, but the copied investigator's own metadata. Restoring the
concrete entity, rather than just its attrs, also preserves `With` metadata.
-}
wylderOriginal :: InvestigatorAttrs -> Card -> Investigator
wylderOriginal attrs card =
  lookupInvestigator (InvestigatorId (toCardCode card)) (investigatorPlayerId attrs)

wylderStandIn :: InvestigatorAttrs -> Metadata -> Card -> Investigator
wylderStandIn attrs meta card =
  let original = wylderOriginal attrs card
      saved = Map.lookup (toCardCode card) (fenrisWylderStates meta) >>= maybeResult
      inner = fromMaybe original saved
   in overAttrs (const (attrs {investigatorMeta = investigatorMeta (toAttrs inner)})) inner

rememberWylder :: Metadata -> Card -> Investigator -> Metadata
rememberWylder meta card inner =
  -- Investigator's JSON parser chooses its concrete type using cardCode. The
  -- running proxy uses Fenris' cardCode, so restore the copied code in the saved
  -- value; otherwise loading it would recursively construct another Fenris.
  let code = toCardCode $ wylderOriginal (toAttrs inner) card
      saved = overAttrs (\a -> a {investigatorCardCode = code}) inner
   in meta {fenrisWylderStates = Map.insert (toCardCode card) (toJSON saved) (fenrisWylderStates meta)}

instance HasModifiersFor Fenris where
  getModifiersFor (Fenris (attrs `With` meta)) = for_ (wylderTop attrs) \card -> do
    let st = getAttrStats . toAttrs $ wylderOriginal attrs card
    modifySelf
      attrs
      [ BaseSkillOf #willpower st.willpower
      , BaseSkillOf #intellect st.intellect
      , BaseSkillOf #combat st.combat
      , BaseSkillOf #agility st.agility
      ]
    getModifiersFor (wylderStandIn attrs meta card)

instance HasAbilities Fenris where
  getAbilities (Fenris (attrs `With` meta)) =
    [ selfAbility_ attrs 9999 (forced $ RoundBegins #when) ]
      <> maybe [] (getAbilities . wylderStandIn attrs meta) (wylderTop attrs)

instance HasChaosTokenValue Fenris where
  getChaosTokenValue iid face (Fenris (attrs `With` meta)) =
    case wylderTop attrs of
      Just card -> getChaosTokenValue iid face (wylderStandIn attrs meta card)
      Nothing -> pure $ ChaosTokenValue face mempty

instance RunMessage Fenris where
  runMessage msg (Fenris (attrs `With` meta)) = runQueueT $ case msg of
    SetupInvestigator iid | attrs `is` iid -> do
      codes <- getCardAttachments iid Cards.fenris
      let chosen = if validWylderSelection codes then codes else []
      investigatorSpecific iid "fenris.chooseWylder" chosen
      -- Finish choosing before normal setup, so the copied text is already
      -- available when opening-hand and permanent-card setup is processed.
      pure $ Fenris $ (attrs & decksL . at WylderDeck ?~ []) `with` Metadata mempty
    InvestigatorSpecific iid "fenris.chooseWylder" value | attrs `is` iid -> do
      let supplied = toResultDefault [] value
      let chosen =
            if validWylderSelection supplied || notNull (wylderCandidates supplied)
              then supplied
              else []
      if validWylderSelection chosen
        then do
          wylder <- shuffleM =<< genCards (mapMaybe (`Map.lookup` Cards.allInvestigatorCards) chosen)
          let attrs' =
                attrs
                  { investigatorSettings =
                      updateCardSetting
                        (toCardCode Cards.fenris)
                        (SetCardSetting CardAttachments chosen)
                        attrs.settings
                  }
          doStep 1 $ SetupInvestigator iid
          pure $ Fenris $ (attrs' & decksL . at WylderDeck ?~ wylder) `with` meta
        else do
          chooseOneM iid do
            cardI18n $ scope "fenris" $ countVar (length chosen + 1) $ questionLabeled "chooseWylder"
            cardsLabeled (wylderCandidates chosen) \card ->
              investigatorSpecific iid "fenris.chooseWylder" (chosen <> [toCardCode card])
          pure $ Fenris $ attrs `with` meta
    DoStep 1 (SetupInvestigator iid) | attrs `is` iid ->
      Fenris . (`with` meta) <$> liftRunMessage (SetupInvestigator iid) attrs
    UseThisAbility _ (isSource attrs -> True) 9999 -> do
      wylder' <- shuffleM (wylderDeck attrs)
      pure $ Fenris $ (attrs & decksL . at WylderDeck ?~ wylder') `with` meta
    _ -> case wylderTop attrs of
      Nothing -> Fenris . (`with` meta) <$> liftRunMessage msg attrs
      Just card -> do
        standIn <- liftRunMessage msg (wylderStandIn attrs meta card)
        let attrs' = (toAttrs standIn) {investigatorMeta = investigatorMeta attrs}
        pure $ Fenris $ attrs' `with` rememberWylder meta card standIn
