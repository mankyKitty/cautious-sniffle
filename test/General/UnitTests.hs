{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module General.UnitTests where

import           Control.Monad                      (void)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.URI.QQ                        (uri)

import           Servant.Client.Generic             (AsClientT)

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

fetchInputElement
  :: Env
  -> W.SessionAPI (AsClientT IO)
  -> IO (W.ElementAPI (AsClientT IO))
fetchInputElement env client = do
  _mkElement (_envWDCore env) client . W.getSuccessValue
    <$> W.findElement client (W.ByCss (input # byId "input-name"))

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
        elemClient <- ioenv >>= \env -> iosess >>= fetchInputElement env . _sessClient
        idAttr <- W.getSuccessValue <$> (W.getElementProperty elemClient "id")
        idAttr @?= "input-name"

    , testCaseSteps "Clear element" $ \step -> do
        step "findElement"
        elemClient <- ioenv >>= \env -> iosess >>= fetchInputElement env . _sessClient

        let
          hasVal v = (W.getSuccessValue <$> W.getElementProperty elemClient "value") >>= (@?= v)
          keys = "taco taco"

        step "element is empty"
        hasVal ""

        step "sendKeys"
        _ <- W.elementSendKeys elemClient (W.ElementSendKeys keys)
        step "element has sent keys"
        hasVal keys

        step "clearElement"
        _ <- W.elementClear elemClient
        step "element is cleared"
        hasVal ""
    ]
  ]
