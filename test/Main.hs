{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import qualified Control.Lens                                            as L

import           Data.List.NonEmpty                                      (NonEmpty (..))

import           Data.Function                                           ((&))
import           Data.Text                                               (Text)
import qualified Data.Text.IO                                            as TIO
import qualified Data.Text.Lazy                                          as T

import qualified Data.Map                                                as Map

import qualified Data.Dependent.Map                                      as DM
import           Data.Dependent.Sum                                      ((==>))

import           Control.Monad.IO.Class                                  (MonadIO,
                                                                          liftIO)
import qualified Protocol.Webdriver.ClientAPI                            as WD
import qualified Protocol.Webdriver.ClientAPI.Types                      as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities         as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox as FF
import           Protocol.Webdriver.ClientAPI.Types.Internal             (Success (unSuccess),
                                                                          WDJson)
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy     as WD
import qualified Protocol.Webdriver.ClientAPI.Types.Session              as WD

import qualified Network.HTTP.Client                                     as HTTP

import           Servant.Client                                          (mkClientEnv,
                                                                          runClientM)
import qualified Servant.Client                                          as C

import           Waargonaut.Types.Json                                   (Json,
                                                                          oat)

import qualified Waargonaut.Encode                                       as E
import qualified Waargonaut.Generic                                      as G
import qualified Waargonaut.Lens                                         as WL

baseUrl :: C.BaseUrl
baseUrl = C.BaseUrl C.Http "localhost" 4444 "/wd/hub"

caps :: NonEmpty WD.Capabilities
caps = ff :| [WD.useChromeOn WD.Linux]
  where
    ffSettings = DM.fromList
        -- I needed to add this as Mozilla removed this setting but my
        -- geckdriver keeps trying to set it to an invalid value and Marionette
        -- crashes, leaving the selenium hanging. :<
      [ FF.FFPrefs ==> FF.GeneralFFPrefs (Map.singleton "app.update.auto" (FF.TextPref "no"))
      ]

    ff = WD.useFirefoxOn WD.Linux
      & DM.insert WD.FirefoxSettings (pure $ ffSettings)

newSess :: WD.NewSession
newSess = WD.NewSession caps Nothing Nothing

printEnc :: (G.JsonEncode WDJson a, MonadIO m) => a -> m ()
printEnc = liftIO . TIO.putStrLn . T.toStrict . E.simplePureEncodeText (G.untag $ G.mkEncoder @WDJson)

newtype SessionId = SessionId { unSessionId :: Text } deriving (Eq, Show)

getSessionId :: Json -> Maybe SessionId
getSessionId = fmap SessionId . L.preview (oat "value" . L._Just . oat "sessionId" . L._Just . WL._String)

qry :: C.ClientM Json
qry = do
  sess <- WD.newSession newSess
  let sid = sess L.^. oat "value" . L._Just . oat "sessionId" . L._Just . WL._String
  liftIO $ print sid
  _ <- WD.navigateTo sid (WD.NavigateTo "http://uitestingplayground.com/textinput")
  textInput <- WD.findElement sid (WD.Locate "css selector" "input[id='newButtonName']")
  _ <- WD.elementSendKeys sid (unSuccess textInput) $ WD.ElementSendKeys "Fred"
  _ <- WD.deleteSession sid
  WD.status

main :: IO ()
main = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  res <- runClientM qry $ mkClientEnv mgr baseUrl
  case res of
    Left err -> print err >> error "Bugger"
    Right a  -> printEnc a
