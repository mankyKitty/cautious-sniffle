{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module General.UnitTests where

import           Control.Arrow                               ((&&&))
import           Control.Monad                               (void)
import           Control.Monad.Catch                         (MonadThrow)
import           Control.Monad.IO.Class                      (MonadIO)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.URI.QQ                                 (uri)

import           Servant.Client.Generic                      (AsClientT)

import qualified Protocol.Webdriver.ClientAPI                as W

import qualified Protocol.Webdriver.ClientAPI.Types          as W
import qualified Protocol.Webdriver.ClientAPI.Types.Internal as W
import qualified Protocol.Webdriver.ClientAPI.Types.Session  as W
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy as W

import           Clay.Elements                                       (input)
import           Clay.Selector                                       (byId, (#))

import           General.ManageDriver
import qualified General.TestAPI                             as API
import           General.Types

openSession :: (MonadThrow m , MonadIO m) => API.WDCore m -> m (W.SessionId, W.SessionAPI (AsClientT m))
openSession core = (id &&& API._mkSession core) . W._sessionId . W.getSuccessValue
  <$> W.newSession (API._core core) chromeSession

closeSession :: (MonadThrow m , MonadIO m) => (W.SessionId, W.SessionAPI (AsClientT m)) -> m ()
closeSession = void . W.deleteSession . snd

fetchInputElement
  :: Env
  -> W.SessionAPI (AsClientT IO)
  -> IO (W.ElementAPI (AsClientT IO))
fetchInputElement env client = do
  API._mkElement (_envWDCore env) client . W.getSuccessValue
    <$> W.findElement client (W.ByCss (input # byId "input-name"))

unitTests :: IO Env -> (Env -> IO ()) -> TestTree
unitTests start stop = withResource start stop $ \ioenv ->
  withResource (ioenv >>= openSession . _envWDCore) closeSession $ \iosess -> testGroup "Unit Tests"
  [ testCase "navigateTo" $ do
      (_, client) <- iosess
      let target = W.WDUri [uri|http://localhost:9999/|]
      currentUri <- W.navigateTo client target >> W.getSuccessValue <$> W.getUrl client
      currentUri @?= target

  , after AllSucceed "navigateTo" $ testGroup "after navigation"
    [ testCase "findElement" $ do
        elemClient <- ioenv >>= \env -> iosess >>= fetchInputElement env . snd
        idAttr <- W.getSuccessValue <$> (W.getElementProperty elemClient "id")
        idAttr @?= "input-name"

    , testCaseSteps "Clear element" $ \step -> do
        step "findElement"
        elemClient <- ioenv >>= \env -> iosess >>= fetchInputElement env . snd

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
