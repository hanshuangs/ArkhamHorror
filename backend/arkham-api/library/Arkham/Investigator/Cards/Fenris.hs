module Arkham.Investigator.Cards.Fenris (fenris) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Investigator (getAttrStats, getCardAttachments)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Deck
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Investigator
import Data.Map.Strict qualified as Map

newtype Fenris = Fenris InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

fenris :: InvestigatorCard Fenris
fenris =
  investigator Fenris Cards.fenris
    $ Stats {health = 7, sanity = 7, willpower = 2, intellect = 2, combat = 2, agility = 2}

-- | The Wylder deck is the four investigators chosen at deck creation.
wylderDeck :: InvestigatorAttrs -> [Card]
wylderDeck = Map.findWithDefault [] WylderDeck . investigatorDecks

wylderTop :: InvestigatorAttrs -> Maybe Card
wylderTop = listToMaybe . wylderDeck

{- | Reproduce the top Wylder card's investigator behaviour, but with Fenris' own
attrs (same id) standing in, so abilities/modifiers stay sourced to Fenris. This
is the same delegation 'TransfiguredForm' uses, minus the stat/sanity/class swap.
-}
wylderStandIn :: InvestigatorAttrs -> Card -> Investigator
wylderStandIn attrs card =
  overAttrs (const attrs)
    $ lookupInvestigator (InvestigatorId (toCardCode card)) (investigatorPlayerId attrs)

instance HasModifiersFor Fenris where
  getModifiersFor (Fenris attrs) = for_ (wylderTop attrs) \card -> do
    let st = getAttrStats . toAttrs $ wylderStandIn attrs card
    modifySelf
      attrs
      [ BaseSkillOf #willpower st.willpower
      , BaseSkillOf #intellect st.intellect
      , BaseSkillOf #combat st.combat
      , BaseSkillOf #agility st.agility
      ]

instance HasAbilities Fenris where
  getAbilities (Fenris attrs) =
    [ selfAbility_ attrs 9999 (forced $ RoundBegins #when) ]
      <> maybe [] (getAbilities . wylderStandIn attrs) (wylderTop attrs)

instance HasChaosTokenValue Fenris where
  getChaosTokenValue iid face (Fenris attrs) =
    case wylderTop attrs of
      Just card -> getChaosTokenValue iid face (wylderStandIn attrs card)
      Nothing -> pure $ ChaosTokenValue face mempty

instance RunMessage Fenris where
  runMessage msg (Fenris attrs) = runQueueT $ case msg of
    SetupInvestigator iid | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      codes <-
        filter (/= toCardCode Cards.fenris)
          <$> getCardAttachments iid Cards.fenris
      wylder <-
        shuffleM
          =<< genCards (mapMaybe (`Map.lookup` Cards.allInvestigatorCards) codes)
      pure $ Fenris $ attrs' & decksL . at WylderDeck ?~ wylder
    UseThisAbility _ (isSource attrs -> True) 9999 -> do
      wylder' <- shuffleM (wylderDeck attrs)
      pure $ Fenris $ attrs & decksL . at WylderDeck ?~ wylder'
    _ -> case wylderTop attrs of
      Nothing -> Fenris <$> liftRunMessage msg attrs
      Just card -> do
        standIn <- liftRunMessage msg (wylderStandIn attrs card)
        pure $ Fenris (toAttrs standIn)
