module Arkham.Investigator.Cards.FenrisSetupSpec (spec) where

import Arkham.Card.Settings
import Arkham.Classes.HasGame (getGame)
import Arkham.Helpers.Investigator (getCardAttachments)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Deck qualified as InvestigatorDeck
import Data.Map.Strict qualified as Map
import TestImport

wylderCodes :: Investigator -> TestAppT [CardCode]
wylderCodes self =
  fieldMap InvestigatorDecks (map toCardCode . Map.findWithDefault [] InvestigatorDeck.WylderDeck) self.id

configureWylder :: Investigator -> [CardCode] -> TestAppT ()
configureWylder self codes = do
  current <- getInvestigator self.id
  void $ updateThis current \attrs ->
    attrs
      { investigatorSettings =
          updateCardSetting
            (toCardCode Investigators.fenris)
            (SetCardSetting CardAttachments codes)
            attrs.settings
      }

chooseWylder :: CardCode -> TestAppT ()
chooseWylder code = chooseOptionMatching "choose Wylder investigator" \case
  CardLabel selected _ _ -> selected == code
  _ -> False

spec :: Spec
spec = describe "Fenris setup" do
  it "uses four valid imported choices without asking again" $ gameTestWith Investigators.fenris \self -> do
    let codes = ["01001", "01002", "01003", "01004"]
    configureWylder self codes
    run $ SetupInvestigator self.id
    actual <- wylderCodes self
    liftIO $ actual `shouldMatchList` codes
    questions <- gameQuestion <$> getGame
    liftIO $ questions `shouldBe` mempty

  it "asks for missing choices and keeps them for the next setup" $ gameTestWith Investigators.fenris \self -> do
    let codes = ["01001", "01002", "01003", "01004"]
    run $ SetupInvestigator self.id
    for_ codes chooseWylder
    actual <- wylderCodes self
    liftIO $ actual `shouldMatchList` codes
    getCardAttachments self.id Investigators.fenris >>= \saved -> liftIO $ saved `shouldBe` codes
    run $ SetupInvestigator self.id
    questions <- gameQuestion <$> getGame
    liftIO $ questions `shouldBe` mempty

  it "asks again when imported choices contain a shared class" $ gameTestWith Investigators.fenris \self -> do
    configureWylder self ["01001", "02001", "01003", "01004"]
    run $ SetupInvestigator self.id
    for_ ["01001", "01002", "01003", "01004"] chooseWylder
    actual <- wylderCodes self
    liftIO $ actual `shouldMatchList` ["01001", "01002", "01003", "01004"]
