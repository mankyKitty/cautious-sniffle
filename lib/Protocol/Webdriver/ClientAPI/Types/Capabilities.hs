{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities 
  ( 
    -- * Types
    Browser (..)
  , Platform (..)
  , PromptHandling (..)
  , BrowserVersionString (..)
  , PageLoad (..)
  , Capability (..)
  , Capabilities

    -- * Encoders/Decoders
  , encPromptHandling, decPromptHandling
  , encPlatform, decPlatform
  , encCapabilities, decCapabilities
  , encBrowser, decBrowser 
  , encPageLoad, decPageLoad
  , capabilityKeyText
  , encodeAtKey

    -- Helper functions
  , firefox
  , chrome
  , asHeadless

    -- * Re-exports
  , module Data.Dependent.Map
  , dmat
  , (==>)
  ) where

import           Control.Lens                                            (cons,
                                                                          (^.))
import           Data.Dependent.Map                                      
import           Data.Dependent.Map.Lens                                 (dmat)
import           Data.Dependent.Sum                                      ((==>))
import           Data.Functor.Alt                                        ((<!>))
import           Data.Functor.Contravariant                              ((>$<))
import           Data.Functor.Identity                                   (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Maybe                                              (fromMaybe)
import           Data.Text                                               (Text)
import qualified Data.Text                                               as T

import qualified Waargonaut.Decode                                       as D
import qualified Waargonaut.Encode                                       as E

import           Protocol.Webdriver.ClientAPI.Types.Internal             (decodeDMap,
                                                                          decodeFromReadUCFirst,
                                                                          dmatKey,
                                                                          encodeDMap,
                                                                          encodeShowToLower,
                                                                          encodeToLower,
                                                                          updateInnerSetting,
                                                                          withString)
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings        (ProxySettings,
                                                                          decProxySettings,
                                                                          encProxySettings)
import           Protocol.Webdriver.ClientAPI.Types.Timeout              (Timeout,
                                                                          decTimeout,
                                                                          encTimeout)

import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Chrome  (ChromeCap (ChrArgs),
                                                                          ChromeCaps,
                                                                          decChromeCaps,
                                                                          encChromeCaps)
import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox (FirefoxCap (FFArgs),
                                                                          FirefoxCaps,
                                                                          decFirefoxCaps,
                                                                          encFirefoxCaps)

data Browser
  = Firefox
  | Chrome
  | Safari
  | IE
  | Opera
  | IPhone
  | IPad
  | Android
  | HtmlUnit
  | PhantomJS
  | Browser String
  deriving (Show, Read, Eq)

decBrowser :: Monad f => D.Decoder f Browser
decBrowser = decodeFromReadUCFirst "Browser" <!> withString else0
  where
    else0 "internet explorer" = pure IE
    else0 s                   = pure (Browser s)

encBrowser :: Applicative f => E.Encoder f Browser
encBrowser = encodeToLower floop
  where
    floop (Browser b) = b
    floop IE          = "internet explorer"
    floop b           = show b

newtype BrowserVersionString = BrowserVersionString
  { unBrowserVersionString :: Text }
  deriving (Show, Eq)

data PageLoad
  = None
  | Eager
  | Normal
  deriving (Show, Read, Eq)

decPageLoad :: Monad f => D.Decoder f PageLoad
decPageLoad = decodeFromReadUCFirst "PageLoad"

encPageLoad :: Applicative f => E.Encoder f PageLoad
encPageLoad = encodeShowToLower

data Platform
  = Linux
  | Unix
  | Windows
  | Vista
  | XP
  | MacOSX
  | Darwin
  | Platform Text
  deriving (Show, Read, Eq)

decPlatform :: Monad f => D.Decoder f Platform
decPlatform = decodeFromReadUCFirst "Platform" <!> (Platform <$> D.text)

encPlatform :: Applicative f => E.Encoder f Platform
encPlatform = encodeToLower $ \case
  Platform p -> T.unpack p
  p          -> show p

data PromptHandling
  = Dismiss
  | Accept
  | DismissNotify
  | AcceptNotify
  | Ignore
  deriving (Show, Eq)

decPromptHandling :: Monad f => D.Decoder f PromptHandling
decPromptHandling = withString $ \case
  "dismiss"            -> pure Dismiss
  "accept"             -> pure Accept
  "dismiss and notify" -> pure DismissNotify
  "accept and notify"  -> pure AcceptNotify
  "ignore"             -> pure Ignore
  _                    -> Left "PromptHandling"

encPromptHandling :: Applicative f => E.Encoder f PromptHandling
encPromptHandling = g >$< E.text
  where
    g Dismiss       = "dismiss"
    g Accept        = "accept"
    g DismissNotify = "dismiss and notify"
    g AcceptNotify  = "accept and notify"
    g Ignore        = "ignore"

data Capability a where
  BrowserName         :: Capability Browser
  BrowserVersion      :: Capability BrowserVersionString
  PlatformName        :: Capability Platform
  AcceptInsecureCerts :: Capability Bool
  PageLoadStrategy    :: Capability PageLoad
  Proxy               :: Capability ProxySettings
  SetWindowRect       :: Capability Bool
  Timeouts            :: Capability Timeout
  StrictFileInteract  :: Capability Bool
  UnhandledBehaviour  :: Capability PromptHandling
  FirefoxSettings     :: Capability FirefoxCaps
  ChromeSettings      :: Capability ChromeCaps

deriving instance Eq a => Eq (Capability a)
deriving instance Ord a => Ord (Capability a)
deriving instance Show a => Show (Capability a)

deriveGShow ''Capability
deriveGEq ''Capability
deriveGCompare ''Capability
deriveEqTagIdentity ''Capability
deriveShowTagIdentity ''Capability

type Capabilities = DMap Capability Identity

capabilityKeyText :: Capability a -> Text
capabilityKeyText k = case k of
  BrowserName         -> "browserName"
  BrowserVersion      -> "browserVersion"
  PlatformName        -> "platformName"
  AcceptInsecureCerts -> "acceptInsecureCerts"
  PageLoadStrategy    -> "pageLoadStrategy"
  Proxy               -> "proxy"
  SetWindowRect       -> "setWindowRect"
  Timeouts            -> "timeouts"
  StrictFileInteract  -> "strictFileInteractability"
  UnhandledBehaviour  -> "unhandledPromptBehavior"
  FirefoxSettings     -> "moz:firefoxOptions"
  ChromeSettings      -> "goog:chromeOptions"

encodeAtKey :: Applicative f => Capability a -> E.Encoder f a
encodeAtKey k = case k of
  BrowserName         -> encBrowser
  BrowserVersion      -> unBrowserVersionString >$< E.text
  PlatformName        -> encPlatform
  AcceptInsecureCerts -> E.bool
  PageLoadStrategy    -> encPageLoad
  Proxy               -> encProxySettings
  SetWindowRect       -> E.bool
  Timeouts            -> encTimeout
  StrictFileInteract  -> E.bool
  UnhandledBehaviour  -> encPromptHandling
  FirefoxSettings     -> encFirefoxCaps
  ChromeSettings      -> encChromeCaps

encCapabilities :: Applicative f => E.Encoder f Capabilities
encCapabilities = encodeDMap $ \k -> (capabilityKeyText k, encodeAtKey k)

decCapabilities :: Monad f => D.Decoder f Capabilities
decCapabilities = decodeDMap
  [ atDM  BrowserName          decBrowser
  , atDM  BrowserVersion       (BrowserVersionString <$> D.text)
  , atDM  PlatformName         decPlatform
  , atDM  AcceptInsecureCerts  D.bool
  , atDM  PageLoadStrategy     decPageLoad
  , proxy0
  , atDM  SetWindowRect        D.bool
  , atDM  Timeouts             decTimeout
  , atDM  StrictFileInteract   D.bool
  , atDM  UnhandledBehaviour   decPromptHandling
  , atDM  FirefoxSettings      decFirefoxCaps
  , atDM  ChromeSettings       decChromeCaps
  ]
  where
    -- the "proxy" value may sometimes be "proxy:{}"
    proxy0 = fromMaybe empty <$> D.try (atDM Proxy decProxySettings)

    atDM = dmatKey capabilityKeyText

firefox :: Capabilities
firefox = singleton BrowserName (pure Firefox)

chrome :: Capabilities
chrome = singleton BrowserName (pure Chrome)

asHeadless :: Capabilities -> Capabilities
asHeadless c0 = case runIdentity <$> c0 ^. dmat BrowserName of
  Nothing      -> c0
  Just Firefox -> updateInnerSetting FirefoxSettings FFArgs [ffopt] (cons ffopt) c0
  Just Chrome  -> updateInnerSetting ChromeSettings ChrArgs [chromeopt] (cons chromeopt) c0
  Just _       -> c0
  where
    hl = "headless"
    ffopt = "-" <> hl
    chromeopt = "--" <> hl
