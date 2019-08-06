{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad                                           (unless,
                                                                          void)
import           Data.Function                                           ((&))
import           Data.Functor                                            ((<&>))
import qualified Data.Text.IO                                            as TIO
import qualified Data.Text.Lazy                                          as T

import           Control.Monad.IO.Class                                  (MonadIO,
                                                                          liftIO)

import qualified Protocol.Webdriver.ClientAPI                            as WD
import           Protocol.Webdriver.ClientAPI.Types                      (WDJson,
                                                                          (~=>))
import qualified Protocol.Webdriver.ClientAPI.Types                      as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities         as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox as FF

import qualified Network.HTTP.Client                                     as HTTP
import           Servant.Client                                          (mkClientEnv,
                                                                          runClientM)
import qualified Servant.Client                                          as C

import qualified Clay.Elements                                           as Clay
import           Clay.Selector                                           (byId,
                                                                          ( # ))
import qualified Text.URI                                                as URI
import qualified Waargonaut.Encode                                       as E
import qualified Waargonaut.Generic                                      as G


baseUrl :: C.BaseUrl
baseUrl = C.BaseUrl C.Http "localhost" 4444
  ""           -- When using geckodriver directly
  -- "/wd/hub" -- When using selenium

firefox :: WD.Capabilities
firefox = WD.firefox
  & WD.PlatformName ~=> WD.Linux
  & WD.FirefoxSettings ~=> ffSettings
  where
    ffSettings = mempty
        -- I needed to add this as Mozilla removed this setting, but my
        -- geckdriver keeps trying to set it to an invalid value and Marionette crashes. :<
      & FF.FFPrefs ~=> FF.newPrefs "app.update.auto" (FF.TextPref "no")
      & FF.FFArgs ~=> ["--headless"]

-- Helper function to be able to print JSON encoded values
printEncodable :: (G.JsonEncode WDJson a, MonadIO m) => a -> m ()
printEncodable = liftIO . TIO.putStrLn . T.toStrict
  . E.simplePureEncodeText (G.untag $ G.mkEncoder @WDJson)

webdriverExample :: C.ClientM ()
webdriverExample = do
  let
    buttonId = "updatingButton"
    textInputId = "newButtonName"
    textInputValue = "Fred"

    core = WD.mkWDCoreClientM

  -- Create the session within the driver
  newSess <- WD.getSuccessValue <$> WD.newSession
    (WD._core core)
    -- we're going to use FireFox so drop in the FireFox capabilities
    (WD.NewSession firefox Nothing Nothing)

  -- We've created a session, so use the session id to access our client.
  let sessClient = WD._mkSession core (WD._sessionId newSess)
  -- NB: This is the only time we'll need to explicity handle the session Id.

  -- We use the 'modern-uri' package to handle the creation of correct URLs
  -- This can also be created inline using quasiquotes: [uri|http://foo.com.au|]
  url <- URI.mkURI "http://uitestingplayground.com/textinput"

  -- Tell the driver to load the page
  WD.navigateTo sessClient (WD.WDUri url)

  -- Some helper functions
  let
    getElem elemTag eid =
      -- Given a CSS identifier, locate an element on the page.
      WD.findElement sessClient (WD.ByCss $ elemTag # byId eid)
      -- Using the found element ID, create a element client for the current session.
      <&> (WD._mkElement core sessClient . WD.getSuccessValue)

    propEq ele prop orig msg = do
      -- Try to retrieve the specified property from the given element.
      p <- WD.getElementProperty ele prop
      -- Check if the value of that property matches our expectations.
      unless (WD.getSuccessValue p == WD.Textual orig) $ error msg

  -- Create element clients for some elements on the web page.
  button <- getElem Clay.button buttonId
  textInput <- getElem Clay.input textInputId

    -- Send some input to the text field
  WD.elementSendKeys textInput $ WD.ElementSendKeys textInputValue
    -- Send a click to the button element
  WD.elementClick button

    -- As per the playground exercise, check that the button has the input text as the new value.
  propEq button "innerHTML" textInputValue "button value mismatch"

    -- Check that the text field has the expected input
  propEq textInput "value" textInputValue "text input value mismatch"

    -- Clear the text field
  WD.elementClear textInput
    -- Check that the text field has been cleared.
  propEq textInput "value" "" "text input should be empty"

    -- Close our session
  void $ WD.deleteSession sessClient

main :: IO ()
main = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  res <- runClientM webdriverExample $ mkClientEnv mgr baseUrl
  case res of
    Left err -> print err >> error "Bugger"
    Right _  -> pure ()
