{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Protocol.Webdriver.ClientAPI.Types where

import           Control.Error                               (headErr)
import           Control.Exception                           (fromException)
import           Data.Bifunctor                              (bimap)
import           Data.Bool                                   (Bool)
import           Data.Functor.Alt                            ((<!>))
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Scientific                             (Scientific)
import qualified Data.Scientific                             as Sci
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Data.Vector                                 (Vector)
import qualified Text.ParserCombinators.ReadP                as R
import           Text.URI                                    (URI)
import qualified Text.URI                                    as URI
import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..),
                                                              gDecoder,
                                                              gEncoder)
import           Waargonaut.Types.Json                       (Json)

import           Generics.SOP.TH                             (deriveGeneric)
import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson, decodeFromReadUCFirst,
                                                              encodeShowToLower,
                                                              singleValueObj,
                                                              trimWaargOpts,
                                                              withString,
                                                              withText)

newtype WDUri = WDUri
  { _unWDUri :: URI }
  deriving (Show, Eq)

encURI :: Applicative f => E.Encoder f WDUri
encURI = singleValueObj "url" (URI.render . _unWDUri >$< E.text)

decURI :: Monad f => D.Decoder f WDUri
decURI = withText (bimap (errText . fromException) WDUri . URI.mkURI)
  where
    errText = maybe
      "WDUri : Unknown Error parsing URI"
      (\(URI.ParseException t _) -> t)

instance JsonDecode WDJson WDUri where mkDecoder = pure decURI
instance JsonEncode WDJson WDUri where mkEncoder = pure encURI

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
  deriving (Read, Show, Eq)

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

data SetWindowRect = SetWindowRect
  { _setWindowRectX      :: Int,
    _setWindowRectY      :: Int,
    _setWindowRectWidth  :: Int,
    _setWindowRectHeight :: Int
  }
  deriving (Show, Eq)
deriveGeneric ''SetWindowRect

instance JsonEncode WDJson SetWindowRect
instance JsonDecode WDJson SetWindowRect

newtype SwitchToFrame = SwitchToFrame
  { _unSwitchToFrame :: Json }
  deriving (Show, Eq)
deriveGeneric ''SwitchToFrame

instance JsonEncode WDJson SwitchToFrame
instance JsonDecode WDJson SwitchToFrame

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
  , _executeScriptArgs   :: (Vector Json)
  }
  deriving (Show, Eq)
deriveGeneric ''ExecuteScript
instance JsonEncode WDJson ExecuteScript
instance JsonDecode WDJson ExecuteScript

data ExecuteAsyncScript = ExecuteAsyncScript
  { _executeAsyncScriptScript :: Text
  , _executeAsyncScriptArgs   :: (Vector Json)
  }
  deriving (Show, Eq)
deriveGeneric ''ExecuteAsyncScript
instance JsonEncode WDJson ExecuteAsyncScript
instance JsonDecode WDJson ExecuteAsyncScript

newtype AddCookie = AddCookie
  { _unAddCookie :: Json }
  deriving (Show, Eq)
deriveGeneric ''AddCookie
instance JsonEncode WDJson AddCookie
instance JsonDecode WDJson AddCookie

newtype PerformActions = PerformActions
  { _unPerformActions :: (Vector Json) }
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
