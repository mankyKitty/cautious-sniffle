{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
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

import qualified Data.Vector as Vector
import qualified Waargonaut.Attoparsec as Waarg
import qualified Waargonaut.Generic as Waarg

import           Clay.Elements                      (input)
import           Clay.Selector                      (byId, selectorFromText, ( # ))

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

fetchElement :: W.LocateUsing -> Env -> W.SessionAPI (AsClientT IO) -> IO (W.ElementAPI (AsClientT IO))
fetchElement elemLoc env client = _mkElement (_envWDCore env) client . W.getSuccessValue
  <$> W.findElement client elemLoc

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

testSelectDropdownOption :: IO Env -> IO Sess -> TestTree
testSelectDropdownOption ioenv iosess = testCaseSteps "Select dropdown element" $ \step -> do
  env <- ioenv
  client <- _sessClient <$> iosess

  let desiredOption = "hamster"

  step "Find <select> element"
  eClient <- fetchElement (W.ByCss $ selectorFromText "#pet-select") env client

  step $ T.unpack $ "Select the " <> desiredOption <> " option"
  _ <- W.selectOption (_envWDCore env) client eClient desiredOption

  step "Get the 'value' of the selected option"
  selected <- W.getSuccessValue <$> W.getElementProperty eClient "value"

  selected @?= W.Textual desiredOption

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
  [ testCase "Decode : (Vector ElementId)" $
      let
        inp = "{\"value\":[{\"element-6066-11e4-a52e-4f735466cecf\":\"9b53211e-dcf7-4d5f-b1a8-3dd0b54ed363\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"647d73e0-73a7-437b-aa39-6bcf7e80deab\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"aa447c8c-8b07-428e-82ca-bed7e310eac7\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"7fd645b0-ffbb-4c27-a553-8157fba64867\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"cdfea5c7-e123-4a12-b71b-e70c6b7aa10e\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"897c53eb-f49b-4da9-904b-412e31d9c45f\"},{\"element-6066-11e4-a52e-4f735466cecf\":\"111cefae-424d-43c3-9729-03c74ab81141\"}]}"
        res = W.Success $ Vector.fromList
          [ W.ElementId "9b53211e-dcf7-4d5f-b1a8-3dd0b54ed363"
          , W.ElementId "647d73e0-73a7-437b-aa39-6bcf7e80deab"
          , W.ElementId "aa447c8c-8b07-428e-82ca-bed7e310eac7"
          , W.ElementId "7fd645b0-ffbb-4c27-a553-8157fba64867"
          , W.ElementId "cdfea5c7-e123-4a12-b71b-e70c6b7aa10e"
          , W.ElementId "897c53eb-f49b-4da9-904b-412e31d9c45f"
          , W.ElementId "111cefae-424d-43c3-9729-03c74ab81141"
          ]
        d = (Waarg.untag $ Waarg.mkDecoder @W.WDJson)
        p = Waarg.pureDecodeAttoparsecByteString d
      in
        p inp @?= Right res

  , testCase "navigateTo" $ do
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

    , testSelectDropdownOption ioenv iosess
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
