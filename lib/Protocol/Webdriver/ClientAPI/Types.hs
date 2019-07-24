{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Protocol.Webdriver.ClientAPI.Types
  ( TakeElementScreenshot (..)
  , CreateWindow (..)
  , ElementSendKeys (..)
  , ExecuteAsyncScript (..)
  , ExecuteScript (..)
  , NewWindow (..)
  , PerformActions (..)
  , SendAlertText (..)
  , SwitchToWindow (..)
  , WDRect (..)

  , SwitchToFrame (..)
  , FrameId (..) 
  , encFrameId
  , decFrameId
  
  , WindowType (..)
  , encWindowType
  , decWindowType

  , WindowHandle (..)
  , encWindowHandle
  , decWindowHandle
  , printWindowHandle
  , checkWindowHandlePattern

  , module Protocol.Webdriver.ClientAPI.Types.Internal
  , module Protocol.Webdriver.ClientAPI.Types.ElementId
  , module Protocol.Webdriver.ClientAPI.Types.Error
  , module Protocol.Webdriver.ClientAPI.Types.LocationStrategy
  , module Protocol.Webdriver.ClientAPI.Types.LogSettings
  , module Protocol.Webdriver.ClientAPI.Types.ProxySettings
  , module Protocol.Webdriver.ClientAPI.Types.Session
  , module Protocol.Webdriver.ClientAPI.Types.Timeout
  , module Protocol.Webdriver.ClientAPI.Types.Cookies
  , module Protocol.Webdriver.ClientAPI.Types.WDUri
  ) where

import           Control.Error                                       (headErr)

import GHC.Word (Word16)

import           Data.Bool                                           (Bool)
import           Data.Functor.Alt                                    ((<!>))
import           Data.Functor.Contravariant                          ((>$<))
import           Data.Scientific                                     (Scientific)
import qualified Data.Scientific                                     as Sci
import           Data.Text                                           (Text)
import qualified Data.Text                                           as T
import           Data.Vector                                         (Vector)

import qualified Text.ParserCombinators.ReadP                        as R

import qualified Waargonaut.Decode                                   as D
import qualified Waargonaut.Encode                                   as E
import           Waargonaut.Generic                                  (JsonDecode (..),
                                                                      JsonEncode (..),
                                                                      gDecoder,
                                                                      gEncoder)
import           Waargonaut.Types.Json                               (Json)

import           Generics.SOP.TH                                     (deriveGeneric)

import           Protocol.Webdriver.ClientAPI.Types.Cookies
import           Protocol.Webdriver.ClientAPI.Types.ElementId
import           Protocol.Webdriver.ClientAPI.Types.Error
import           Protocol.Webdriver.ClientAPI.Types.Internal
import           Protocol.Webdriver.ClientAPI.Types.LocationStrategy
import           Protocol.Webdriver.ClientAPI.Types.LogSettings
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings
import           Protocol.Webdriver.ClientAPI.Types.Session
import           Protocol.Webdriver.ClientAPI.Types.Timeout
import           Protocol.Webdriver.ClientAPI.Types.WDUri

-- Each browsing context has an associated window handle which uniquely identifies it. This must be a String and must not be "current".
data WindowHandle
-- The web window identifier is the string constant "window-fcc6-11e5-b4f8-330a88ab9d7f".
  = WebWindowId Text
-- The web frame identifier is the string constant "frame-075b-4da1-b6ba-e579c2d3230a".
  | WebFrameId Text
  | NumericId Scientific
  deriving (Show, Eq)

printWindowHandle :: WindowHandle -> Text
printWindowHandle (WebWindowId t) = t
printWindowHandle (WebFrameId  t) = t
printWindowHandle (NumericId   s) = T.pack $ Sci.formatScientific Sci.Fixed Nothing s

checkWindowHandlePattern :: Text -> Text -> Bool
checkWindowHandlePattern pfx inp =
  let
    pfxLen   = T.length pfx
    len n s  = T.length s == n
    pattOk x = case T.splitOn "-" x of
      [a,b,c,d] -> len 4 a && len 4 b && len 4 c && len 12 d
      _         -> False
  in
    T.isPrefixOf pfx inp && pattOk (T.drop pfxLen inp)

decWindowHandle :: Monad f => D.Decoder f WindowHandle
decWindowHandle = numericId <!> (withText $ \s ->
    if checkWindowHandlePattern "window-" s then pure (WebWindowId s)
    else if checkWindowHandlePattern "frame-" s then pure (WebFrameId s)
    else Left s
  )
  where
    numericId = withString $ \s -> fmap (NumericId . fst) . headErr (T.pack s)
      $ R.readP_to_S Sci.scientificP s

encWindowHandle :: Applicative f => E.Encoder f WindowHandle
encWindowHandle = printWindowHandle >$< E.text

instance JsonEncode WDJson WindowHandle where mkEncoder = pure encWindowHandle
instance JsonDecode WDJson WindowHandle where mkDecoder = pure decWindowHandle

newtype SwitchToWindow = SwitchToWindow
  { _switchToWindowHandle :: WindowHandle }
  deriving (Show, Eq)
deriveGeneric ''SwitchToWindow

instance JsonEncode WDJson SwitchToWindow where
  mkEncoder = gEncoder $ trimWaargOpts "_switchToWindow"

instance JsonDecode WDJson SwitchToWindow where
  mkDecoder = gDecoder $ trimWaargOpts "_switchToWindow"

data WindowType
  = Window
  | Tab
  deriving (Read, Show, Eq, Bounded, Enum)

encWindowType :: Applicative f => E.Encoder f WindowType
encWindowType = encodeShowToLower

decWindowType :: Monad f => D.Decoder f WindowType
decWindowType = decodeFromReadUCFirst "WindowType"

instance JsonEncode WDJson WindowType where mkEncoder = pure encWindowType
instance JsonDecode WDJson WindowType where mkDecoder = pure decWindowType

newtype CreateWindow = CreateWindow
  { _unCreateWindow :: WindowType }
  deriving (Show, Eq)
deriveGeneric ''CreateWindow

instance JsonEncode WDJson CreateWindow
instance JsonDecode WDJson CreateWindow

data NewWindow = NewWindow
  { _newWindowHandle :: WindowHandle
  , _newWindowType   :: WindowType
  }
  deriving (Show, Eq)
deriveGeneric ''NewWindow

instance JsonEncode WDJson NewWindow where
  mkEncoder = gEncoder $ trimWaargOpts "_newWindowHandle"

instance JsonDecode WDJson NewWindow where
  mkDecoder = gDecoder $ trimWaargOpts "_newWindowHandle"

data WDRect = WDRect
  { _windowRectX      :: Int,
    _windowRectY      :: Int,
    _windowRectWidth  :: Int,
    _windowRectHeight :: Int
  }
  deriving (Show, Eq)
deriveGeneric ''WDRect

instance JsonEncode WDJson WDRect
instance JsonDecode WDJson WDRect

data FrameId
  = NullFrame
  | Number Word16
  | FrameElement ElementId
  deriving (Show, Eq)
deriveGeneric ''FrameId

encFrameId :: Applicative f => E.Encoder f FrameId
encFrameId = E.encodeA $ \case
  NullFrame      -> E.runEncoder E.null ()
  Number n       -> E.runEncoder E.integral n
  FrameElement e -> E.runEncoder encElementId e

decFrameId :: Monad f => D.Decoder f FrameId
decFrameId = (NullFrame <$ D.null) <!> (Number <$> D.integral) <!> (FrameElement <$> decElementId)

instance JsonEncode WDJson FrameId where mkEncoder = pure encFrameId
instance JsonDecode WDJson FrameId where mkDecoder = pure decFrameId

newtype SwitchToFrame = SwitchToFrame
  { _switchToFrameId :: FrameId }
  deriving (Show, Eq)
deriveGeneric ''SwitchToFrame

instance JsonEncode WDJson SwitchToFrame where
  mkEncoder = gEncoder $ trimWaargOpts "_switchToFrame"
instance JsonDecode WDJson SwitchToFrame where
  mkDecoder = gDecoder $ trimWaargOpts "_switchToFrame"

newtype ElementSendKeys = ElementSendKeys
  { _elementSendKeysValue :: Text }
  deriving (Show, Eq)
deriveGeneric ''ElementSendKeys

instance JsonEncode WDJson ElementSendKeys where
  mkEncoder = pure $ E.mapLikeObj $ \esk ->
    E.atKey' "value" (E.list E.text) [_elementSendKeysValue esk] .
    -- This is the new Webdriver way but chromedriver still requires a list.
    E.atKey' "text" E.text (_elementSendKeysValue esk)

newtype TakeElementScreenshot = TakeElementScreenshot
  { _unTakeElementScreenshot :: Maybe Bool }
  deriving (Show, Eq)
deriveGeneric ''TakeElementScreenshot
instance JsonEncode WDJson TakeElementScreenshot
instance JsonDecode WDJson TakeElementScreenshot

data ExecuteScript = ExecuteScript
  { _executeScriptScript :: Text
  , _executeScriptArgs   :: Vector Json
  }
  deriving (Show, Eq)
deriveGeneric ''ExecuteScript
instance JsonEncode WDJson ExecuteScript
instance JsonDecode WDJson ExecuteScript

data ExecuteAsyncScript = ExecuteAsyncScript
  { _executeAsyncScriptScript :: Text
  , _executeAsyncScriptArgs   :: Vector Json
  }
  deriving (Show, Eq)
deriveGeneric ''ExecuteAsyncScript
instance JsonEncode WDJson ExecuteAsyncScript
instance JsonDecode WDJson ExecuteAsyncScript

newtype PerformActions = PerformActions
  { _unPerformActions :: Vector Json }
  deriving (Show, Eq)
deriveGeneric ''PerformActions
instance JsonEncode WDJson PerformActions
instance JsonDecode WDJson PerformActions

newtype SendAlertText = SendAlertText
  { _unSendAlertText :: Text }
  deriving (Show, Eq)
deriveGeneric ''SendAlertText
instance JsonEncode WDJson SendAlertText
instance JsonDecode WDJson SendAlertText
