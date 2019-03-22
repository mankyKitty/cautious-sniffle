{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Lens as L

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as T

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.IO.Class (liftIO)
import qualified Protocol.Webdriver.ClientAPI.Types as WD
import qualified Protocol.Webdriver.ClientAPI as WD

import qualified Network.HTTP.Client as HTTP

import Servant.Client (runClientM, mkClientEnv)
import qualified Servant.Client as C

import Waargonaut.Types.Json (Json,oat)

import qualified Waargonaut.Lens as WL
import qualified Waargonaut.Encode as E
import qualified Waargonaut.Generic as G

baseUrl :: C.BaseUrl
baseUrl = C.BaseUrl C.Http "localhost" 4444 ""

data Capabilities = Capabilities
  { _cababilityBrowserName :: Text
  , _cababilityBrowserPlatform :: Text
  }

encCapa :: Applicative f => E.Encoder f Capabilities
encCapa = E.mapLikeObj $ \c ->
  E.atKey' "browserName" E.text (_cababilityBrowserName c) .
  E.atKey' "platformName" E.text (_cababilityBrowserPlatform c)

caps :: [Capabilities]
caps = [Capabilities "firefox" "linux"]

newSess :: WD.NewSession
newSess = WD.NewSession (E.asJson' (E.list encCapa) caps)

qry :: C.ClientM Json
qry = do
  liftIO . TIO.putStrLn . T.toStrict $ E.simplePureEncodeText (G.untag $ G.mkEncoder @WD.WDJson) newSess
  sess <- WD.newSession newSess
  let sid = sess L.^. oat "sessionId" . L._Just . WL._String
  _ <- WD.navigateTo sid (WD.NavigateTo "http://uitestingplayground.com/textinput")
  s <- WD.status
  _ <- WD.deleteSession sid
  pure s

main :: IO ()
main = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  res <- runClientM qry $ mkClientEnv mgr baseUrl
  case res of
    Left err -> error (show err)
    Right a -> TIO.putStrLn . T.toStrict $ E.simplePureEncodeText E.json a

capabilities :: Map Text Text
capabilities = Map.fromList
  [ ("browserName", "chrome") --   string  Identifies the user agent.
  , ("browserVersion", "") --  string  Identifies the version of the user agent.
  , ("platformName", "") --  string  Identifies the operating system of the endpoint node.
  , ("acceptInsecureCerts", "") --   boolean   Indicates whether untrusted and self-signed TLS certificates are implicitly trusted on navigation for the duration of the session.
  , ("pageLoadStrategy", "") --  string  Defines the current session’s page load strategy.
  , ("proxy", "") --   JSON Object   Defines the current session’s proxy configuration.
  , ("setWindowRect", "") --   boolean   Indicates whether the remote end supports all of the resizing and repositioning commands.
  , ("timeouts", "") --  JSON Object   Describes the timeouts imposed on certain session operations.
  , ("strictFileInteractability", "") --   boolean   Defines the current session’s strict file interactability.
  , ("unhandledPromptBehavior", "") --   string  Describes the current session’s user prompt handler. Defaults to the dismiss and notify state. 
  ]