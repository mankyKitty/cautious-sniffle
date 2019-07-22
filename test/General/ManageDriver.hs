{-# LANGUAGE RankNTypes #-}
module General.ManageDriver
  ( startDriver
  , stopDriver
  , localSelenium
  , Env (..)
  , manageDriverAndServer
  ) where

import           Test.Tasty                   (TestTree, askOption)

import           Data.Foldable                (traverse_)
import           Data.Maybe                   (fromMaybe)
import           Data.Maybe                   (isJust)

import           Control.Concurrent           (ThreadId, forkIO, killThread,
                                               threadDelay)
import           System.IO                    (openTempFile)
import           System.Process               (ProcessHandle, spawnCommand,
                                               terminateProcess)

import           Servant.Client               (BaseUrl, ClientEnv)
import qualified Servant.Client               as Servant

import qualified Network.HTTP.Client          as HTTP

import qualified Protocol.Webdriver.ClientAPI as W

import           General.TestAPI              (WDCore, mkWDCoreTest)
import           General.TestOpts             (OverrideWDUrl (..))
import           General.Webserver

localSelenium :: IO ProcessHandle
localSelenium = do
  (f, _) <- openTempFile "/tmp/" "SELENIUM_WD_LOG.log"
  putStrLn $ "Selenium log file: " <> f
  spawnCommand $ "selenium-server -port 4444 -debug -log " <> f

-- If we've started a driver then stop it
stopDriver :: Maybe ProcessHandle -> IO ()
stopDriver = traverse_ terminateProcess

-- Create a managed selenium server if needed
startDriver :: Maybe BaseUrl -> IO (Maybe ProcessHandle)
startDriver existingUrl = if isJust existingUrl then pure Nothing else do
  d <- pure <$> localSelenium
  d <$ (putStrLn "Pause to allow selenium to start" >> threadDelay 2000000)

data Env = Env
  { _envTestWebServer :: ThreadId
  , _envDriverProcess :: Maybe ProcessHandle
  , _envEnv           :: ClientEnv
  , _envWDCore        :: WDCore IO
  }

manageDriverAndServer :: (IO Env -> (Env -> IO ()) -> TestTree) -> TestTree
manageDriverAndServer f = askOption $ \(OverrideWDUrl existingWDUrl) ->
  let
    wdClientUrl =
      fromMaybe W.defaultWebdriverClient existingWDUrl

    start = do
      env <- flip Servant.mkClientEnv wdClientUrl <$> HTTP.newManager HTTP.defaultManagerSettings
      Env
        <$> forkIO testWebServer
        <*> startDriver existingWDUrl
        <*> pure env
        <*> pure (mkWDCoreTest env) -- (RunWD $ flip Servant.runClientM env)

    stop (Env srv sel _ _) =
      stopDriver sel >> killThread srv
  in
    f start stop
