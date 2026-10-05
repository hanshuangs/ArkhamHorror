module Arkham.Enemy.CardDefs.Fenris where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

-- | Fenris' signature weakness. "Program" is a fan-made trait, and the
-- Cyberspace/Meatspace spawn locations are not in the engine, so it spawns the
-- normal way for a weakness enemy (engaged with the investigator).
ireress :: CardDef
ireress =
  signature "99100"
    $ (weakness "99102" "Ireress")
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, HomebrewTrait "Program"]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 3)]
      }
