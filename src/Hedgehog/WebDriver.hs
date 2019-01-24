{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver (main) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)

import           System.Exit            (exitFailure, exitSuccess)

import qualified Test.WebDriver         as WD
import           Test.WebDriver.Session (WDSession, getSession)

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

data ButtonText (v :: * -> *) =
  ButtonText Text Bool
  deriving (Show, Eq)

instance HTraversable ButtonText where
  htraverse _ (ButtonText t inp) = pure (ButtonText t inp)

newtype TextInput (v :: * -> *) =
  TextInput Text
  deriving (Show, Eq)

instance HTraversable TextInput where
  htraverse _ (TextInput t) = pure (TextInput t)

data ClickButton (v :: * -> *) =
  ClickButton
  deriving (Show, Eq)

instance HTraversable ClickButton where
  htraverse _ _ = pure ClickButton

btnId, inputField :: WD.Selector

btnId      = WD.ByCSS "#updatingButton"
inputField = WD.ByCSS "input[id ='newButtonName']"

withElem
  :: ( MonadTest m
     , MonadIO m
     )
  => WDSession
  -> WD.Selector
  -> (WD.Element -> WD.WD a)
  -> m a
withElem sess sel f = evalIO . WD.runWD sess $
  WD.findElem sel >>= f

cInputText
  :: ( MonadGen n
     , MonadTest m
     , MonadIO m
     )
  => WDSession
  -> Command n m ButtonText
cInputText sess =
  let
    gen _ = Just $ TextInput <$> Gen.text (Range.linear 0 50) Gen.alphaNum

    execute (TextInput t) = withElem sess inputField $ \inp ->
      WD.sendKeys t inp >> fromMaybe "bugger" <$> WD.attr inp "value"
  in
    Command gen execute
    [ Update $ \(ButtonText _ _) (TextInput i) _                    -> ButtonText i True
    , Ensure $ \(ButtonText _ _) (ButtonText n _) (TextInput i) out -> do
        i === out
        n === out
    ]

cClickButton
  :: ( MonadGen n
     , MonadTest m
     , MonadIO m
     )
  => WDSession
  -> Command n m ButtonText
cClickButton sess =
  let
    gen _ = Just $ Gen.constant ClickButton

    execute _ = withElem sess btnId $ \b ->
      WD.click b >> WD.getText b
  in
    Command gen execute
    [ Require $ \(ButtonText _ hasBeenInput) _ -> hasBeenInput
    , Ensure $ \(ButtonText t _) _ _ out       -> t === out
    ]

prop_button_updates :: WDSession -> Property
prop_button_updates sess = withTests 5 . property $ do
  let
    cmds = ($ sess) <$> [cInputText, cClickButton]
    st   = ButtonText mempty False

  actions <- forAll $ Gen.sequential (Range.linear 1 5) st cmds

  -- Refresh the page to reset everything
  evalIO $ WD.runWD sess WD.refresh

  executeSequential st actions

ff  = WD.useBrowser WD.firefox WD.defaultConfig

main :: IO ()
main = do
  r <- WD.runSession ff $ do
    WD.openPage "http://uitestingplayground.com/textinput"
    sess <- getSession

    r <- checkSequential $ Group "Wut"
      [ ("Button updates to be text input", prop_button_updates sess)
      ]

    WD.closeSession
    pure r

  if r then exitSuccess else exitFailure
