{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module General.UnitTests where

import Control.Exception (displayException)
import Control.Applicative (liftA2)
import           Control.Monad                      (void)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Text                          (Text)
import qualified Data.Text                          as T

import           Text.URI.QQ                        (uri)

import qualified Servant.Client as C
import           Servant.Client.Generic             (AsClientT)

import           Protocol.Webdriver.ClientAPI       (WDCore (..))
import qualified Protocol.Webdriver.ClientAPI       as W
import qualified Protocol.Webdriver.ClientAPI.Types as W

import           Clay.Elements                      (input)
import           Clay.Selector                      (byId, ( # ))

import           General.Types

import qualified Example

nameInputId :: Text
nameInputId = "input-name"

occupationInputId :: Text
occupationInputId = "input-occupation"

openSession :: WDCore IO -> IO Sess
openSession core = (\i -> Sess i (_mkSession core i)) . W._sessionId . W.getSuccessValue
  <$> W.newSession (_core core) firefoxSession

closeSession :: Sess -> IO ()
closeSession = void . W.deleteSession . _sessClient

fetchInputElement :: Text -> Env -> W.SessionAPI (AsClientT IO) -> IO (W.ElementAPI (AsClientT IO))
fetchInputElement elemId env client = _mkElement (_envWDCore env) client . W.getSuccessValue
  <$> W.findElement client (W.ByCss (input # byId elemId))

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

testPerformKeyActions :: IO Sess -> TestTree
testPerformKeyActions iosess = testCaseSteps "Perform keyboard actions" $ \step -> do
  client <- _sessClient <$> iosess
  let
    findElemId css =
      W.getSuccessValue <$> W.findElement client (W.ByCss css)

    pressTab = do
      step "TAB key down" *> W.performActions client (W.PerformActions [tabKey W.KeyDown])
      step "TAB key released" *> W.releaseActions client

  -- reset the page
  W.refresh client
  
  nameInp <- findElemId $ input # byId nameInputId
  occInp <- findElemId $ input # byId occupationInputId

  pressTab
  step "Get selected element"
  (W.getSuccessValue <$> W.getActiveElement client) >>= (@?= nameInp)

  pressTab
  step "Get selected element"
  (W.getSuccessValue <$> W.getActiveElement client) >>= (@?= occInp)
  where
    tabKey ka = W.Action W.KeyAction (W.ActionId "keyboard") Nothing [ka W.tab]

testExampleCode :: IO Env -> (Env -> IO ()) -> TestTree
testExampleCode start stop = withResource start stop $ \ioenv -> testCase "Example Code Works" $
  ioenv
  >>= C.runClientM Example.webdriverExample . _envEnv
  >>= either (assertFailure . displayException) pure

unitTests :: IO Env -> (Env -> IO ()) -> TestTree
unitTests start stop = withResource start stop $ \ioenv ->
  withResource (ioenv >>= openSession . _envWDCore) closeSession $ \iosess -> testGroup "Unit Tests"
  [ testCase "navigateTo" $ do
      client <- _sessClient <$> iosess
      let target = W.WDUri [uri|http://localhost:9999/|]
      currentUri <- W.navigateTo client target >> W.getSuccessValue <$> W.getCurrentUrl client
      currentUri @?= target

  , after AllSucceed "navigateTo" $ testGroup "after navigation"
    [ testCase "findElement" $ do
        elemClient <- fetchElemClient ioenv iosess
        idAttr <- W.getSuccessValue <$> W.getElementProperty elemClient "id"
        idAttr @?= W.Textual "input-name"

    , testCase "Input to non-text type input" $ do
        numI <- uncurry (fetchInputElement "input-age") =<< liftA2 (,) ioenv (_sessClient <$> iosess) 
        W.elementSendKeys numI (W.ElementSendKeys "33") *> hasTextVal numI "33"

    , testCaseSteps "getProperty Types" $ \step -> do
        elemClient <- fetchElemClient ioenv iosess
        let getProp = fmap W.getSuccessValue . W.getElementProperty elemClient
        step "textual" *> getProp "id" >>= (@?= W.Textual "input-name")
        step "numeric" *> getProp "tabIndex" >>= (@?= W.Numeric 0)
        step "boolean" *> getProp "required" >>= (@?= W.Boolean True)

    , testCaseSteps "Clear element" $ \step -> do
        let keys = "taco taco"
        elemClient <- step "findElement" *> fetchElemClient ioenv iosess
        step "element is empty"      *> hasTextVal elemClient mempty
        step "sendKeys"              *> W.elementSendKeys elemClient (W.ElementSendKeys keys)
        step "element has sent keys" *> hasTextVal elemClient keys
        step "clearElement"          *> W.elementClear elemClient
        step "element is cleared"    *> hasTextVal elemClient mempty

    , testPrompt iosess
    , testPerformKeyActions iosess

    , testCase "sendKeys with a backspace" $ do
        elemClient <- fetchElemClient ioenv iosess
        _ <- W.elementSendKeys elemClient (W.ElementSendKeys $ "abc" <> T.cons W.backspace "def")
        hasTextVal elemClient "abdef"
    ]
  ]
  where
    fetchElemClient ioenv iosess =
      liftA2 (,) ioenv (_sessClient <$> iosess)
      >>= uncurry (fetchInputElement nameInputId)

    hasTextVal cli v =
      (W.getSuccessValue <$> W.getElementProperty cli "value") >>=
        (@?= W.Textual v)
