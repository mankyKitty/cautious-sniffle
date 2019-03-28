{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types where

import           Control.Exception                           (fromException)
import           Control.Monad.Error.Lens                    (throwing)
import           Data.Bool                                   (Bool)
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Text                                   (Text)
import           Data.Vector                                 (Vector)
import qualified GHC.Generics                                as GHC
import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Decode.Error                     as DE
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (Generic,
                                                              HasDatatypeInfo,
                                                              JsonDecode (..),
                                                              JsonEncode (..),
                                                              Tagged (..))
import           Waargonaut.Types.Json                       (Json)

import           Text.URI                                    (URI)
import qualified Text.URI                                    as URI

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson,
                                                              singleValueObj)

newtype WDUri = WDUri
  { _unWDUri :: URI }
  deriving (Show, Eq)

encURI :: Applicative f => E.Encoder f WDUri
encURI = singleValueObj "url" (URI.render . _unWDUri >$< E.text)

decURI :: Monad f => D.Decoder f WDUri
decURI = D.text >>= either
  (throwing DE._ConversionFailure . errText . fromException)
  (pure . WDUri) .
  URI.mkURI
  where
    errText Nothing                         = "WDUri : Unknown Error parsing URI"
    errText (Just (URI.ParseException t _)) = t

instance JsonDecode WDJson WDUri where mkDecoder = pure decURI
instance JsonEncode WDJson WDUri where mkEncoder = pure encURI

newtype SwitchToWindow = SwitchToWindow
  { _unSwitchToWindow :: Text }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SwitchToWindow
instance Generic SwitchToWindow
instance JsonEncode WDJson SwitchToWindow
instance JsonDecode WDJson SwitchToWindow

newtype CreateWindow = CreateWindow
  { _unCreateWindow :: Text }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo CreateWindow
instance Generic CreateWindow
instance JsonEncode WDJson CreateWindow
instance JsonDecode WDJson CreateWindow

data SetWindowRect = SetWindowRect
  { _setWindowRectX      :: Int,
    _setWindowRectY      :: Int,
    _setWindowRectWidth  :: Int,
    _setWindowRectHeight :: Int
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SetWindowRect
instance Generic SetWindowRect
instance JsonEncode WDJson SetWindowRect
instance JsonDecode WDJson SetWindowRect

newtype SwitchToFrame = SwitchToFrame
  { _unSwitchToFrame :: Json }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SwitchToFrame
instance Generic SwitchToFrame
instance JsonEncode WDJson SwitchToFrame
instance JsonDecode WDJson SwitchToFrame

newtype ElementSendKeys = ElementSendKeys
  { _elementSendKeysValue :: Text }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ElementSendKeys
instance Generic ElementSendKeys

instance JsonEncode WDJson ElementSendKeys where
  mkEncoder = Tagged $ E.mapLikeObj $ \esk ->
    E.atKey' "value" (E.list E.text) [_elementSendKeysValue esk] .
    -- This appears to be the new W3C Webdriver way but the chromedriver still
    -- appears to require the list input.
    E.atKey' "text" E.text (_elementSendKeysValue esk)

newtype TakeElementScreenshot = TakeElementScreenshot
  { _unTakeElementScreenshot :: Maybe Bool }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo TakeElementScreenshot
instance Generic TakeElementScreenshot
instance JsonEncode WDJson TakeElementScreenshot
instance JsonDecode WDJson TakeElementScreenshot

data ExecuteScript = ExecuteScript
  { _executeScriptScript :: Text
  , _executeScriptArgs :: (Vector Json)
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ExecuteScript
instance Generic ExecuteScript
instance JsonEncode WDJson ExecuteScript
instance JsonDecode WDJson ExecuteScript

data ExecuteAsyncScript = ExecuteAsyncScript
  { _executeAsyncScriptScript :: Text
  , _executeAsyncScriptArgs :: (Vector Json)
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ExecuteAsyncScript
instance Generic ExecuteAsyncScript
instance JsonEncode WDJson ExecuteAsyncScript
instance JsonDecode WDJson ExecuteAsyncScript

newtype AddCookie = AddCookie
  { _unAddCookie :: Json }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo AddCookie
instance Generic AddCookie
instance JsonEncode WDJson AddCookie
instance JsonDecode WDJson AddCookie

newtype PerformActions = PerformActions
  { _unPerformActions :: (Vector Json) }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo PerformActions
instance Generic PerformActions
instance JsonEncode WDJson PerformActions
instance JsonDecode WDJson PerformActions

newtype SendAlertText = SendAlertText
  { _unSendAlertText :: Text }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SendAlertText
instance Generic SendAlertText
instance JsonEncode WDJson SendAlertText
instance JsonDecode WDJson SendAlertText
