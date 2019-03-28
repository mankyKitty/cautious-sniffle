{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad                                           (void)
import           Data.Function                                           ((&))
import qualified Data.Text.IO                                            as TIO
import qualified Data.Text.Lazy                                          as T

import           Control.Monad.IO.Class                                  (MonadIO,
                                                                          liftIO)
import qualified Protocol.Webdriver.ClientAPI                            as WD
import qualified Protocol.Webdriver.ClientAPI.Types                      as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities         as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox as FF
import           Protocol.Webdriver.ClientAPI.Types.Internal             (Value (unValue),
                                                                          WDJson,
                                                                          (~=>))
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy     as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Session              as WD

import qualified Network.HTTP.Client                                     as HTTP

import           Servant.Client                                          (mkClientEnv,
                                                                          runClientM)
import qualified Servant.Client                                          as C

import qualified Waargonaut.Encode                                       as E
import qualified Waargonaut.Generic                                      as G
import qualified Text.URI as URI
import           Clay.Elements                                           (input)
import           Clay.Selector                                           (byId,
                                                                          ( # ))

baseUrl :: C.BaseUrl
baseUrl = C.BaseUrl C.Http "localhost" 4444 "/wd/hub"

-- I needed to add this as Mozilla removed this setting but my
-- geckdriver keeps trying to set it to an invalid value and Marionette
-- crashes, leaving the selenium hanging. :<
firefox :: WD.Capabilities
firefox = WD.firefox
  & WD.PlatformName ~=> WD.Linux
  & WD.FirefoxSettings ~=> ffSettings
  where
    ffSettings = mempty 
      & FF.FFPrefs ~=> FF.newPrefs "app.update.auto" (FF.TextPref "no")

newSess :: WD.Capabilities -> WD.NewSession
newSess cap = WD.NewSession cap Nothing Nothing

printEncodable :: (G.JsonEncode WDJson a, MonadIO m) => a -> m ()
printEncodable = liftIO 
  . TIO.putStrLn 
  . T.toStrict 
  . E.simplePureEncodeText (G.untag $ G.mkEncoder @WDJson)

qry :: C.ClientM ()
qry = do
  url <- URI.mkURI "http://uitestingplayground.com/textinput"

  sess <- WD.newSession (newSess firefox)

  let sid = WD.unSessionId . WD._sessionId $ unValue sess

  _         <- WD.navigateTo sid (WD.WDUri url)
  textInput <- WD.findElement sid . WD.ByCss $ input # byId "newButtonName"
  _         <- WD.elementSendKeys sid (unValue textInput) $ WD.ElementSendKeys "Fred"

  void $ WD.deleteSession sid

main :: IO ()
main = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  res <- runClientM qry $ mkClientEnv mgr baseUrl
  case res of
    Left err -> print err >> error "Bugger"
    Right _  -> pure ()
