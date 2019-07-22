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

import           General.ManageDriver
import qualified General.TestAPI                             as API
import           General.Types

openSession :: (MonadThrow m , MonadIO m) => API.WDCore m -> m (W.SessionId, W.SessionAPI (AsClientT m))
openSession core = (id &&& API._mkSession core) . W._sessionId . W.unValue
  <$> W.newSession (API._core core) chromeSession

closeSession :: (MonadThrow m , MonadIO m) => (W.SessionId, W.SessionAPI (AsClientT m)) -> m ()
closeSession = void . W.deleteSession . snd

unitTests :: IO Env -> (Env -> IO ()) -> TestTree
unitTests start stop = withResource start stop $ \ioenv ->
  withResource (ioenv >>= openSession . _envWDCore) closeSession $ \iosess -> testGroup "Unit Tests"
  [ testCase "navigateTo" $ do
      (_, client) <- iosess
      let target = W.WDUri [uri|http://localhost:9999/|]
      currentUri <- W.navigateTo client target >> W.unValue <$> W.getUrl client
      currentUri @?= target

  , after AllSucceed "navigateTo" $ testGroup "after navigation"
    []
  ]
