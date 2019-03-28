{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities.Chrome where

import           Control.Lens                                     (preview,
                                                                   ( # ), _1)
import           Control.Monad.Error.Lens                         (throwing)
import           Data.Functor.Contravariant                       ((>$<))

import           Data.Text                                        (Text)
import qualified Data.Text                                        as T

import           Data.Maybe                                       (fromMaybe)

import           Data.Dependent.Map                               (DMap)
import           Data.Functor.Identity                            (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Map                                         (Map)
import qualified Data.Map                                         as Map

import           Protocol.Webdriver.ClientAPI.Types.Internal      (Base64,
                                                                   decBase64,
                                                                   decodeDMap,
                                                                   dmatKey,
                                                                   encBase64,
                                                                   encodeDMap)
import           Protocol.Webdriver.ClientAPI.Types.LogSettings   (LogSettings, decLogSettings,
                                                                   encLogSettings)
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings (HostPort,
                                                                   decHostPort,
                                                                   encHostPort)

import qualified Waargonaut.Decode                                as D
import qualified Waargonaut.Decode.Error                          as DE
import qualified Waargonaut.Encode                                as E

import           Waargonaut.Types.JObject                         (MapLikeObj, fromMapLikeObj,
                                                                   _MapLikeObj)
import           Waargonaut.Types.Json                            (Json, _JObj)
import           Waargonaut.Types.Whitespace                      (WS)

-- Capabilities from http://chromedriver.chromium.org/capabilities

newtype ChromeLocalState = ChromeLocalState
  { unChromeLocalState :: MapLikeObj WS Json
  }
  deriving (Show, Eq)

decChromeLocalState :: Monad f => D.Decoder f ChromeLocalState
decChromeLocalState = D.json >>= handleErr . preview (_JObj . _1 . _MapLikeObj)
  where
    handleErr = maybe
      (throwing DE._ConversionFailure "ChromeLocalState")
      (pure . ChromeLocalState)

encChromeLocalState :: Applicative f => E.Encoder f ChromeLocalState
encChromeLocalState = ((_JObj #) . (,mempty) . fromMapLikeObj . unChromeLocalState) >$< E.json

newtype ChromePrefs = ChromePrefs
  { unChromePrefs :: Map Text Text
  }
  deriving (Show, Eq)

decChromePrefs :: Monad f => D.Decoder f ChromePrefs
decChromePrefs = ChromePrefs . Map.fromList <$> D.objectAsKeyValues D.text D.text

encChromePrefs :: Applicative f => E.Encoder f ChromePrefs
encChromePrefs = unChromePrefs >$< E.mapToObj E.text id

newtype DebuggerAddr = DebuggerAddr
  { unDebuggerAddr :: HostPort
  }
  deriving (Show, Eq)

decDebuggerAddr :: Monad f => D.Decoder f DebuggerAddr
decDebuggerAddr = DebuggerAddr <$> decHostPort

encDebuggerAddr :: Applicative f => E.Encoder f DebuggerAddr
encDebuggerAddr = unDebuggerAddr >$< encHostPort

newtype ExcludeSwitches = ExcludeSwitches
  { unExcludeSwitches :: [Text]
  }
  deriving (Show, Eq)

decExcludeSwitches :: Monad f => D.Decoder f ExcludeSwitches
decExcludeSwitches = ExcludeSwitches <$> D.list D.text

encExcludeSwitches :: Applicative f => E.Encoder f ExcludeSwitches
encExcludeSwitches = fmap (\t -> fromMaybe t $ T.stripPrefix "--" t)
  . unExcludeSwitches
  >$< E.list E.text

newtype ChromeExtension = ChromeExtension
  { unChromeExtension :: Base64
  }
  deriving (Show, Eq)

decChromeExtension :: Monad f => D.Decoder f ChromeExtension
decChromeExtension = ChromeExtension <$> decBase64

encChromeExtension :: Applicative f => E.Encoder f ChromeExtension
encChromeExtension = unChromeExtension >$< encBase64

newtype ChromeMobileEmu = ChromeMobileEmu
  { unChromeMobileEmu :: Map Text Text
  }
  deriving (Show, Eq)

decChromeMobileEmu :: Monad f => D.Decoder f ChromeMobileEmu
decChromeMobileEmu = ChromeMobileEmu . Map.fromList <$> D.objectAsKeyValues D.text D.text

newtype WindowTypes = WindowTypes
  { unWindowTypes :: [Text]
  }
  deriving (Show, Eq)

data ChromeCap a where
  ChrArgs             :: ChromeCap [Text]
  ChrBinary           :: ChromeCap FilePath
  ChrExtensions       :: ChromeCap [ChromeExtension]
  ChrLocalState       :: ChromeCap ChromeLocalState
  ChrPrefs            :: ChromeCap ChromePrefs
  ChrDetach           :: ChromeCap Bool
  ChrDebuggerAddr     :: ChromeCap DebuggerAddr
  ChrExcludeSwitches  :: ChromeCap ExcludeSwitches
  ChrMinidumpPath     :: ChromeCap FilePath
  ChrMobileEmulation  :: ChromeCap ChromeMobileEmu
  ChrPerfLoggingPrefs :: ChromeCap LogSettings
  ChrWindowTypes      :: ChromeCap WindowTypes

type ChromeCaps = DMap ChromeCap Identity

deriving instance Eq a => Eq (ChromeCap a)
deriving instance Show a => Show (ChromeCap a)
deriving instance Ord a => Ord (ChromeCap a)

deriveGShow ''ChromeCap
deriveGEq ''ChromeCap
deriveGCompare ''ChromeCap
deriveEqTagIdentity ''ChromeCap
deriveShowTagIdentity ''ChromeCap

chromeCapKeys :: ChromeCap a -> Text
chromeCapKeys ChrArgs             = "args"
chromeCapKeys ChrBinary           = "binary"
chromeCapKeys ChrExtensions       = "extensions"
chromeCapKeys ChrLocalState       = "localState"
chromeCapKeys ChrPrefs            = "prefs"
chromeCapKeys ChrDetach           = "detach"
chromeCapKeys ChrDebuggerAddr     = "debuggerAddress"
chromeCapKeys ChrExcludeSwitches  = "excludeSwitches"
chromeCapKeys ChrMinidumpPath     = "minidumpPath"
chromeCapKeys ChrMobileEmulation  = "mobileEmulation"
chromeCapKeys ChrWindowTypes      = "windowTypes"
chromeCapKeys ChrPerfLoggingPrefs = "perfLoggingPrefs"

chromeCapEnc :: Applicative f => ChromeCap a -> E.Encoder f a
chromeCapEnc ChrArgs             = E.list E.text
chromeCapEnc ChrBinary           = E.string
chromeCapEnc ChrExtensions       = E.list encChromeExtension
chromeCapEnc ChrLocalState       = encChromeLocalState
chromeCapEnc ChrPrefs            = encChromePrefs
chromeCapEnc ChrDetach           = E.bool
chromeCapEnc ChrDebuggerAddr     = encDebuggerAddr
chromeCapEnc ChrExcludeSwitches  = encExcludeSwitches
chromeCapEnc ChrMinidumpPath     = E.string
chromeCapEnc ChrMobileEmulation  = unChromeMobileEmu >$< E.mapToObj E.text id
chromeCapEnc ChrWindowTypes      = unWindowTypes >$< E.list E.text
chromeCapEnc ChrPerfLoggingPrefs = encLogSettings

decChromeCaps :: Monad f => D.Decoder f ChromeCaps
decChromeCaps = decodeDMap
  [ atDM ChrArgs (D.list D.text)
  , atDM ChrBinary D.string
  , atDM ChrExtensions (D.list decChromeExtension)
  , atDM ChrLocalState decChromeLocalState
  , atDM ChrPrefs decChromePrefs
  , atDM ChrDetach D.bool
  , atDM ChrDebuggerAddr decDebuggerAddr
  , atDM ChrExcludeSwitches decExcludeSwitches
  , atDM ChrMinidumpPath D.string
  , atDM ChrMobileEmulation decChromeMobileEmu
  , atDM ChrPerfLoggingPrefs decLogSettings
  , atDM ChrWindowTypes (WindowTypes <$> D.list D.text)
  ]
  where
    atDM = dmatKey chromeCapKeys

encChromeCaps :: Applicative f => E.Encoder f ChromeCaps
encChromeCaps = encodeDMap $ \k -> (chromeCapKeys k, chromeCapEnc k)
