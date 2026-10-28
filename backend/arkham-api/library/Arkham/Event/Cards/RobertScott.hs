module Arkham.Event.Cards.RobertScott where

import Arkham.Criteria qualified as Criteria
import Arkham.Event.Cards.Import

jetStrike :: CardDef
jetStrike = signature "a5a24167-f20e-40b9-92ed-551374516701"
  $ (event "50571516-0d4a-46da-b8c3-6ba26b2baaec" "Jet Strike" 2 Neutral)
    { cdCardTraits = singleton Tactic
    , cdSkills = [#combat, #combat, #wild]
    , cdActions = #fight
    , cdAdditionalCost = Just $ OptionalCost $ ExileFromCost $ InHandOf ForPlay You <> basic (oneOf $ map CardWithLevel [0 .. 5])
    }

putOutTheFire :: CardDef
putOutTheFire = signature "a5a24167-f20e-40b9-92ed-551374516701"
  $ (event "6c392683-6c14-4c45-a5d6-25d58d433433" "Put out the Fire" 0 Neutral)
    { cdCardTraits = setFromList [Spirit, Bold]
    , cdSkills = [#willpower, #willpower]
    , cdCriteria = Just $ Criteria.FirstAction <> exists (You <> InvestigatorWithMetaKey "robertScottHasExiledCards")
    }
