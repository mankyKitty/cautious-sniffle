{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad                                           (void,unless)
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

import           Clay.Elements                                           (input)
import           Clay.Selector                                           (byId,
                                                                          ( # ))
import qualified Text.URI                                                as URI
import qualified Waargonaut.Encode                                       as E
import qualified Waargonaut.Generic                                      as G

import qualified Protocol.Webdriver.ClientAPI.Record as F

baseUrl :: C.BaseUrl
baseUrl = C.BaseUrl C.Http "localhost" 4444 "/wd/hub"

firefox :: WD.Capabilities
firefox = WD.firefox
  & WD.PlatformName ~=> WD.Linux
  & WD.FirefoxSettings ~=> ffSettings
  where
    ffSettings = mempty
        -- I needed to add this as Mozilla removed this setting but my
        -- geckdriver keeps trying to set it to an invalid value and Marionette
        -- crashes, leaving the selenium hanging. :<
      & FF.FFPrefs ~=> FF.newPrefs "app.update.auto" (FF.TextPref "no")
      & FF.FFArgs ~=> ["--headless"]

newSess :: WD.Capabilities -> WD.NewSession
newSess cap = WD.NewSession cap Nothing Nothing

printEncodable :: (G.JsonEncode WDJson a, MonadIO m) => a -> m ()
printEncodable = liftIO . TIO.putStrLn . T.toStrict
  . E.simplePureEncodeText (G.untag $ G.mkEncoder @WDJson)

usingSession :: F.InSession -> C.ClientM ()
usingSession F.InSession {..} = do
  let
    buttonId = "newButtonName"
    textInputValue = "Fred"

  _         <- navigateTo (WD.WDUri url)
  textInput <- findElement . WD.ByCss $ input # byId buttonId
  _         <- elementSendKeys (unValue textInput) $ WD.ElementSendKeys textInputValue

  attr     <- getElementAttribute (unValue textInput) "id"
  unless (unValue attr == buttonId) $ error "attribute mismatch"

  prop     <- getElementProperty (unValue textInput) "value"
  unless (unValue prop == textInputValue) $ error "text input value mismatch"

  void $ deleteSession

qry :: C.ClientM ()
qry = do
  let api = F.mkWDApi
  url <- URI.mkURI "http://uitestingplayground.com/textinput"

  sess <- F.newSession api (newSess firefox)

  let
    sid = WD._sessionId $ unValue sess
    currSess = F.withSession api sid

    buttonId = "newButtonName"
    textInputValue = "Fred"

  _         <- F.navigateTo currSess (WD.WDUri url)
  textInput <- F.findElement currSess . WD.ByCss $ input # byId buttonId
  _         <- F.elementSendKeys currSess (unValue textInput) $ WD.ElementSendKeys textInputValue

  attr     <- F.getElementAttribute currSess (unValue textInput) "id"
  unless (unValue attr == buttonId) $ error "attribute mismatch"

  prop     <- F.getElementProperty currSess (unValue textInput) "value"
  unless (unValue prop == textInputValue) $ error "text input value mismatch"

  void $ F.deleteSession currSess

main :: IO ()
main = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  res <- runClientM qry $ mkClientEnv mgr baseUrl
  case res of
    Left err -> print err >> error "Bugger"
    Right _  -> pure ()
