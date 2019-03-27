{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities.Chrome where

import           Control.Lens                                     (( # ))
import           Data.Functor.Contravariant                       ((>$<))

import           Data.Text                                        (Text)
import qualified Data.Text                                        as T

import           Data.Maybe                                       (fromMaybe)

import           Data.Dependent.Map                               (DMap)
import           Data.Functor.Identity                            (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Map                                         (Map)

import           Protocol.Webdriver.ClientAPI.Types.Internal      (Base64,
                                                                   encodeBase64,
                                                                   encodeDMap)
import           Protocol.Webdriver.ClientAPI.Types.LogSettings   (LogSettings, encodeLogSettings)
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings (HostPort, encodeHostPort)

import qualified Waargonaut.Encode                                as E

import           Waargonaut.Types.JObject                         (MapLikeObj, fromMapLikeObj)
import           Waargonaut.Types.Json                            (Json, _JObj)
import           Waargonaut.Types.Whitespace                      (WS)

-- Capabilities from http://chromedriver.chromium.org/capabilities

newtype ChromeLocalState = ChromeLocalState
  { unChromeLocalState :: MapLikeObj WS Json
  }
  deriving (Show, Eq)

encodeChromeLocalState :: Applicative f => E.Encoder f ChromeLocalState
encodeChromeLocalState = ((_JObj #) . (,mempty) . fromMapLikeObj . unChromeLocalState) >$< E.json

newtype ChromePrefs = ChromePrefs
  { unChromePrefs :: Map Text Text
  }
  deriving (Show, Eq)

newtype DebuggerAddr = DebuggerAddr
  { unDebuggerAddr :: HostPort
  }
  deriving (Show, Eq)

newtype ExcludeSwitches = ExcludeSwitches
  { unExcludeSwitches :: [Text]
  }
  deriving (Show, Eq)

encodeExcludeSwitches :: Applicative f => E.Encoder f ExcludeSwitches
encodeExcludeSwitches = fmap (\t -> fromMaybe t $ T.stripPrefix "--" t)
  . unExcludeSwitches
  >$< E.list E.text

newtype ChromeExtension = ChromeExtension
  { unChromeExtension :: Base64
  }
  deriving (Show, Eq)

newtype ChromeMobileEmu = ChromeMobileEmu
  { unChromeMobileEmu :: Map Text Text
  }
  deriving (Show, Eq)

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

deriveGShow ''ChromeCap
deriveGEq ''ChromeCap
deriveGCompare ''ChromeCap

type ChromeCaps = DMap ChromeCap Identity

encodeChromeCaps :: Applicative f => E.Encoder f ChromeCaps
encodeChromeCaps = encodeDMap $ \case
  ChrArgs            -> ("args", E.list E.text)
  ChrBinary          -> ("binary", E.string)
  ChrExtensions      -> ("extensions", E.list (unChromeExtension >$< encodeBase64))
  ChrLocalState      -> ("localState", encodeChromeLocalState)
  ChrPrefs           -> ("prefs", unChromePrefs >$< E.mapToObj E.text id)
  ChrDetach          -> ("detach", E.bool)
  ChrDebuggerAddr    -> ("debuggerAddress", unDebuggerAddr >$< encodeHostPort)
  ChrExcludeSwitches -> ("excludeSwitches", encodeExcludeSwitches)
  ChrMinidumpPath    -> ("minidumpPath", E.string)
  ChrMobileEmulation -> ("mobileEmulation", unChromeMobileEmu >$< E.mapToObj E.text id)
  ChrWindowTypes     -> ("windowTypes", unWindowTypes >$< E.list E.text)
  ChrPerfLoggingPrefs -> ("perfLoggingPrefs", encodeLogSettings)
