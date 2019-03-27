{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types where

import           Data.Bool                                   (Bool)
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Scientific                             (Scientific)
import           Data.Text                                   (Text)
import           Data.Vector                                 (Vector)
import qualified GHC.Generics                                as GHC
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (Generic,
                                                              HasDatatypeInfo,
                                                              JsonDecode (..),
                                                              JsonEncode (..),
                                                              Tagged (..))
import           Waargonaut.Types.Json                       (Json)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)

data SetTimeouts = SetTimeouts
  { _setTimeoutsImplicit :: Maybe Scientific
  , _setTimeoutsPageLoad :: Maybe Scientific
  , _setTimeoutsScript   :: Maybe Scientific
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SetTimeouts
instance Generic SetTimeouts
instance JsonEncode WDJson SetTimeouts
instance JsonDecode WDJson SetTimeouts

newtype NavigateTo = NavigateTo
  { _unNavigateTo :: Text
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo NavigateTo
instance Generic NavigateTo
instance JsonEncode WDJson NavigateTo where
  mkEncoder = Tagged (E.mapLikeObj $ E.atKey' "url" (_unNavigateTo >$< E.text))

newtype SwitchToWindow = SwitchToWindow{_unSwitchToWindow
                                        :: Text}
                         deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SwitchToWindow
instance Generic SwitchToWindow
instance JsonEncode WDJson SwitchToWindow
instance JsonDecode WDJson SwitchToWindow

newtype CreateWindow = CreateWindow{_unCreateWindow
                                    :: Text}
                       deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo CreateWindow
instance Generic CreateWindow
instance JsonEncode WDJson CreateWindow
instance JsonDecode WDJson CreateWindow

data SetWindowRect = SetWindowRect
  { _setWindowRectX      :: (Maybe Scientific),
    _setWindowRectY      :: (Maybe Scientific),
    _setWindowRectWidth  :: (Maybe Scientific),
    _setWindowRectHeight :: (Maybe Scientific)
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SetWindowRect
instance Generic SetWindowRect
instance JsonEncode WDJson SetWindowRect
instance JsonDecode WDJson SetWindowRect

newtype SwitchToFrame = SwitchToFrame{_unSwitchToFrame
                                      :: Json}
                        deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SwitchToFrame

instance Generic SwitchToFrame

instance JsonEncode WDJson SwitchToFrame

instance JsonDecode WDJson SwitchToFrame

data FindElement = FindElement{_findElementUsing
                               :: Text,
                               _findElementValue :: Text}
                   deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo FindElement

instance Generic FindElement

instance JsonEncode WDJson FindElement

instance JsonDecode WDJson FindElement

data FindElementFromElement = FindElementFromElement{_findElementFromElementUsing
                                                     :: Text,
                                                     _findElementFromElementValue :: Text}
                              deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo FindElementFromElement

instance Generic FindElementFromElement

instance JsonEncode WDJson FindElementFromElement

instance JsonDecode WDJson FindElementFromElement

data FindElementsFromElement = FindElementsFromElement{_findElementsFromElementUsing
                                                       :: Text,
                                                       _findElementsFromElementValue :: Text}
                               deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo FindElementsFromElement

instance Generic FindElementsFromElement

instance JsonEncode WDJson FindElementsFromElement

instance JsonDecode WDJson FindElementsFromElement

newtype ElementSendKeys = ElementSendKeys
  { _elementSendKeysValue :: Text
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ElementSendKeys
instance Generic ElementSendKeys

instance JsonEncode WDJson ElementSendKeys where
  mkEncoder = Tagged $ E.mapLikeObj $ \esk ->
    E.atKey' "value" (E.list E.text) [_elementSendKeysValue esk] .
    E.atKey' "text" E.text (_elementSendKeysValue esk)

newtype TakeElementScreenshot = TakeElementScreenshot
  { _unTakeElementScreenshot :: Maybe Bool
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo TakeElementScreenshot

instance Generic TakeElementScreenshot

instance JsonEncode WDJson TakeElementScreenshot

instance JsonDecode WDJson TakeElementScreenshot

data FindElements = FindElements{_findElementsUsing
                                 :: Text,
                                 _findElementsValue :: Text}
                    deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo FindElements

instance Generic FindElements

instance JsonEncode WDJson FindElements

instance JsonDecode WDJson FindElements

data ExecuteScript = ExecuteScript{_executeScriptScript
                                   :: Text,
                                   _executeScriptArgs :: (Vector Json)}
                     deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ExecuteScript

instance Generic ExecuteScript

instance JsonEncode WDJson ExecuteScript

instance JsonDecode WDJson ExecuteScript

data ExecuteAsyncScript = ExecuteAsyncScript{_executeAsyncScriptScript
                                             :: Text,
                                             _executeAsyncScriptArgs :: (Vector Json)}
                          deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo ExecuteAsyncScript

instance Generic ExecuteAsyncScript

instance JsonEncode WDJson ExecuteAsyncScript

instance JsonDecode WDJson ExecuteAsyncScript

newtype AddCookie = AddCookie{_unAddCookie ::
                              Json}
                    deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo AddCookie

instance Generic AddCookie

instance JsonEncode WDJson AddCookie

instance JsonDecode WDJson AddCookie

newtype PerformActions = PerformActions{_unPerformActions
                                        :: (Vector Json)}
                         deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo PerformActions

instance Generic PerformActions

instance JsonEncode WDJson PerformActions

instance JsonDecode WDJson PerformActions

newtype SendAlertText = SendAlertText{_unSendAlertText
                                      :: Text}
                        deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo SendAlertText

instance Generic SendAlertText

instance JsonEncode WDJson SendAlertText

instance JsonDecode WDJson SendAlertText
