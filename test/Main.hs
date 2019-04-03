{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO (openTempFile)
import qualified System.Process                                  as Proc

import           Control.Concurrent                              (forkIO,
                                                                  killThread,
                                                                  threadDelay)
import           Control.Exception                               (bracket)
import           Control.Monad                                   (void)
import           Control.Monad.IO.Class                          (MonadIO)

import qualified Servant.Client                                  as Servant

import qualified Network.HTTP.Client                             as HTTP
import qualified Web.Scotty                                      as W

import           Hedgehog                                        (Group (..),
                                                                  MonadTest,
                                                                  Property,
                                                                  PropertyT,
                                                                  checkSequential,
                                                                  evalEither,
                                                                  evalIO,
                                                                  executeSequential,
                                                                  forAll,
                                                                  property,
                                                                  withTests)

import qualified Hedgehog.Gen                                    as Gen
import qualified Hedgehog.Range                                  as Range

import qualified Protocol.Webdriver.ClientAPI                    as W
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities as W
import qualified Protocol.Webdriver.ClientAPI.Types.Internal     as W
import qualified Protocol.Webdriver.ClientAPI.Types.Session      as W

import           Commands
import           Types

htmlunitSession :: W.NewSession
htmlunitSession =
  W.NewSession
    -- (W.singleton W.BrowserName (pure W.HtmlUnit))  -- Browser config
    (W.asHeadless W.chrome)
    Nothing                                        -- username
    Nothing                                        -- password

webdriverStateTest
  :: WDRun (PropertyT IO)
  -> W.Session
  -> Property
webdriverStateTest runner sess = withTests 5 . property $ do
  let
    initialModel = Model
      False
      Nothing

    mk c =
      c runner (W._sessionId sess)

    commands =
      [ mk cFindElement
      , mk cNavigateTo
      , mk cSendKeys
      ]

  actions <- forAll $ Gen.sequential (Range.linear 1 10) initialModel commands
  executeSequential initialModel actions

main :: IO Bool
main = do
  env <- flip Servant.mkClientEnv W.defaultWebdriverClient
    <$> HTTP.newManager HTTP.defaultManagerSettings

  let
    stopSeleniumAndWebServer (web, sel) = do
      killThread web
      Proc.terminateProcess sel

    startSeleniumAndWebServer = do
      sel <- localSelenium
      putStrLn "Pause to allow selenium to start" >> threadDelay 2000000
      ws <- forkIO babbyTestWebServer
      pure (ws, sel)

    runForEither a =
      Servant.runClientM a env

    runner =
      WDRun (runWDAction env) (evalIO . runForEither)

  bracket startSeleniumAndWebServer stopSeleniumAndWebServer .
    const $ do
      sess <- either (error . show) (pure . W.unValue) =<< runForEither (W.newSession htmlunitSession)
      b <- checkSequential $ Group "WebDriver Tests" [
        ("Webdriver command sequences", webdriverStateTest runner sess)
        ]
      void $ runForEither (W.deleteSession (W._sessionId sess))
      pure b

runWDAction
  :: ( MonadIO m
     , MonadTest m
     )
  => Servant.ClientEnv
  -> Servant.ClientM b
  -> m b
runWDAction env action =
  evalIO (Servant.runClientM action env) >>= evalEither

babbyTestWebServer :: IO ()
babbyTestWebServer = W.scotty 9999 $ do
  W.get "/" (W.file "test/test-webpage.html")
  W.get "/taco" (W.file "test/test-taco-webpage.html")

localSelenium
  :: IO Proc.ProcessHandle
localSelenium = do
  (f, _) <- openTempFile "/tmp/" "SELENIUM_WD_LOG.log"
  putStrLn $ "Selenium log file: " <> f
  Proc.spawnCommand $ "selenium-server -port 4444 -debug -log " <> f
