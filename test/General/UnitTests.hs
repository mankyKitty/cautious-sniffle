{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module General.UnitTests where

import           Control.Monad                      (void)

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Text.URI.QQ                        (uri)

import           Servant.Client.Generic             (AsClientT)

import           Protocol.Webdriver.ClientAPI       (WDCore (..))
import qualified Protocol.Webdriver.ClientAPI       as W
import qualified Protocol.Webdriver.ClientAPI.Types as W

import           Clay.Elements                      (input)
import           Clay.Selector                      (byId, ( # ))

import           General.Types

openSession :: WDCore IO -> IO Sess
openSession core = (\i -> Sess i (_mkSession core i)) . W._sessionId . W.getSuccessValue
  <$> W.newSession (_core core) chromeSession

closeSession :: Sess -> IO ()
closeSession = void . W.deleteSession . _sessClient

fetchInputElement :: Env -> W.SessionAPI (AsClientT IO) -> IO (W.ElementAPI (AsClientT IO))
fetchInputElement env client = _mkElement (_envWDCore env) client . W.getSuccessValue
  <$> W.findElement client (W.ByCss (input # byId "input-name"))

testPrompt :: IO Sess -> TestTree
testPrompt iosess = testGroup "Prompt actions"
  [ testAfterTrigger "trigger and dismiss" $ fmap W.getSuccessValue . W.dismissAlert
  , testAfterTrigger "trigger and accept" $ fmap W.getSuccessValue . W.acceptAlert
  , testAfterTrigger "trigger, send text, and accept" $ \client -> do
      _ <- W.sendAlertText client $ W.SendAlertText promptResponse
      void $ W.acceptAlert client
  , testAfterTrigger "trigger, get text and dismiss" $ \client -> do
      t <- W.getAlertText client
      _ <- W.dismissAlert client
      W.getSuccessValue t @?= promptText
  ]
  where
    promptText     = "New prompt, who dis?"
    promptResponse = "oh no"

    testAfterTrigger n t = testCase n $ do
      client <- _sessClient <$> iosess
      _ <- triggerPrompt client
      t client

    triggerPrompt client = client <$ W.executeScript client
      (W.ExecuteScript ("window.prompt(\"" <> promptText <> "\")") mempty)

unitTests :: IO Env -> (Env -> IO ()) -> TestTree
unitTests start stop = withResource start stop $ \ioenv ->
  withResource (ioenv >>= openSession . _envWDCore) closeSession $ \iosess -> testGroup "Unit Tests"
  [ testCase "navigateTo" $ do
      client <- _sessClient <$> iosess
      let target = W.WDUri [uri|http://localhost:9999/|]
      currentUri <- W.navigateTo client target >> W.getSuccessValue <$> W.getUrl client
      currentUri @?= target

  , after AllSucceed "navigateTo" $ testGroup "after navigation"
    [ testCase "findElement" $ do
        elemClient <- fetchElemClient ioenv iosess
        idAttr <- W.getSuccessValue <$> W.getElementProperty elemClient "id"
        idAttr @?= W.Textual "input-name"

    , testCaseSteps "getProperty Types" $ \step -> do
        elemClient <- fetchElemClient ioenv iosess
        let getProp = fmap W.getSuccessValue . W.getElementProperty elemClient
        step "textual" >> getProp "id" >>= (@?= W.Textual "input-name")
        step "numeric" >> getProp "tabIndex" >>= (@?= W.Numeric 0)
        step "boolean" >> getProp "required" >>= (@?= W.Boolean True)
        step "other (serialised as JSON)" >> getProp "children" >>= \o -> case o of
          W.OtherVal _ -> pure ()
          _            -> assertFailure "Not a JSON value!"

    , testCaseSteps "Clear element" $ \step -> do
        step "findElement"
        elemClient <- fetchElemClient ioenv iosess

        let keys = "taco taco"

        step "element is empty"
        hasTextVal elemClient ""

        step "sendKeys"
        _ <- W.elementSendKeys elemClient (W.ElementSendKeys keys)
        step "element has sent keys"
        hasTextVal elemClient keys

        step "clearElement"
        _ <- W.elementClear elemClient
        step "element is cleared"
        hasTextVal elemClient ""

    , testPrompt iosess

    , testCase "sendKeys with a backspace" $ do
        elemClient <- fetchElemClient ioenv iosess
        _ <- W.elementSendKeys elemClient (W.ElementSendKeys $ "abc" <> T.cons W.backspace "def")
        hasTextVal elemClient "abdef"
    ]
  ]
  where
    fetchElemClient ioenv iosess = ioenv >>= \env -> 
      iosess >>= fetchInputElement env . _sessClient

    hasTextVal cli v = 
      (W.getSuccessValue <$> W.getElementProperty cli "value") >>= 
        (@?= W.Textual (TE.encodeUtf8 v))
