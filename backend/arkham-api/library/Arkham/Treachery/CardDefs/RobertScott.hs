module Arkham.Treachery.CardDefs.RobertScott where

import Arkham.Treachery.CardDefs.Import

cantSaveThemAll :: CardDef
cantSaveThemAll = signature "a5a24167-f20e-40b9-92ed-551374516701"
  $ (weakness "675f833b-dd06-4784-b785-c9f3762fbef2" "Can't Save Them All")
    {cdCardTraits = singleton Blunder}
