module Arkham.Investigator.Wylder (
  wylderCandidates,
  validWylderSelection,
  wylderClasses,
  compatibleWylderInvestigator,
) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Name (toTitle)
import Arkham.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- Neutral cards have no class. A multiclass investigator occupies every one
-- of their classes, rather than just the first symbol on the card.
wylderClasses :: CardDef -> Set ClassSymbol
wylderClasses = Set.filter (`notElem` [Neutral, Mythos]) . cdClassSymbols

compatibleWylderInvestigator :: [CardDef] -> CardDef -> Bool
compatibleWylderInvestigator chosen candidate = all compatible chosen
 where
  compatible other =
    toTitle candidate /= toTitle other
      && Set.null (Set.intersection (wylderClasses candidate) (wylderClasses other))

isWylderInvestigator :: CardDef -> Bool
isWylderInvestigator def =
  cdCardCode def `notElem` [cdCardCode Cards.fenris, "10016a", "10016b", "11068b"]
    && isNothing (cdEncounterSet def)

-- The player-investigator registry excludes the prologue/campaign identities.
-- Also exclude Hank's resolute forms and Lost Homunculus: none can be chosen
-- when creating a deck. Reprints and promos offer the same identity only once.
allWylderInvestigators :: [CardDef]
allWylderInvestigators =
  sortOn toTitle
    $ Map.elems
    $ Map.fromListWith (\_ original -> original)
      [ (canonicalCardCode def, def)
      | def <- Map.elems Cards.allInvestigatorCards
      , isWylderInvestigator def
      ]

resolveWylderSelection :: [CardCode] -> Maybe [CardDef]
resolveWylderSelection codes = do
  defs <- traverse (`Map.lookup` Cards.allInvestigatorCards) codes
  guard $ all isWylderInvestigator defs
  let valid [] = True
      valid (def : rest) = compatibleWylderInvestigator rest def && valid rest
  guard $ valid defs
  pure defs

-- Invalid partial selections have no successors. Callers should restart an
-- invalid imported selection instead of silently accepting unknown card codes.
wylderCandidates :: [CardCode] -> [CardDef]
wylderCandidates chosenCodes = fromMaybe [] do
  guard $ length chosenCodes < 4
  chosen <- resolveWylderSelection chosenCodes
  pure $ filter (compatibleWylderInvestigator chosen) allWylderInvestigators

validWylderSelection :: [CardCode] -> Bool
validWylderSelection codes = length codes == 4 && isJust (resolveWylderSelection codes)
