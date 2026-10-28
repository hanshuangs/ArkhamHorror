module Arkham.Asset.Cards.Fenris where

import Arkham.Asset.Cards.Import

-- | "Permanent. Fenris deck only." — the club itself, not a card you pay for.
wyldside :: CardDef
wyldside =
  signature "net12"
    $ permanent
    $ (asset "twoa10002733" "Wyldside" 0 Neutral)
      { cdCardTraits = singleton Connection
      }
