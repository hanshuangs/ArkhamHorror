module Arkham.Investigator.RobertScott where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorMeta))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Target

-- History and the remaining exiled cards are deliberately separate: putting a
-- card in the victory display must not reduce Jet Strike's damage bonus.
data RobertScottState = RobertScottState
  { exiledCards :: [Card]
  , recoveredCards :: [CardId]
  , elderSignPending :: Bool
  , fireVictoryCard :: Maybe (CardId, Int)
  , fireRewarded :: Bool
  , pendingExiles :: [Target]
  }
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

emptyRobertScottState :: RobertScottState
emptyRobertScottState = RobertScottState [] [] False Nothing False []

robertScottState :: Value -> RobertScottState
robertScottState = toResultDefault emptyRobertScottState

getRobertScottState :: HasGame m => InvestigatorId -> m RobertScottState
getRobertScottState iid = robertScottState <$> field InvestigatorMeta iid

availableExiledCards :: RobertScottState -> [Card]
availableExiledCards state = filter ((`notElem` state.recoveredCards) . toCardId) state.exiledCards

exilableLevels :: [Int] -> CardMatcher
exilableLevels = oneOf . map CardWithLevel

robertScottCardCodes :: [CardCode]
robertScottCardCodes =
  [ "a5a24167-f20e-40b9-92ed-551374516701"
  , "50571516-0d4a-46da-b8c3-6ba26b2baaec"
  , "6c392683-6c14-4c45-a5d6-25d58d433433"
  , "675f833b-dd06-4784-b785-c9f3762fbef2"
  ]
