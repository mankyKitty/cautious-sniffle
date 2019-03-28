{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities where

import           Control.Monad.Error.Lens                                (throwing)
import           Data.Functor.Contravariant                              ((>$<))
import           Data.Text                                               (Text)

import           Data.Dependent.Map                                      (DMap)
import qualified Data.Dependent.Map                                      as DM
import           Data.Dependent.Sum                                      (EqTag (..),
                                                                          ShowTag (..),
                                                                          (==>))
import           Data.Functor.Identity                                   (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Maybe                                              (fromMaybe)

import qualified Waargonaut.Decode                                       as D
import qualified Waargonaut.Decode.Error                                 as DE
import qualified Waargonaut.Encode                                       as E

import           Protocol.Webdriver.ClientAPI.Types.Internal             (decodeDMap,
                                                                          dmatKey,
                                                                          encodeDMap,
                                                                          encodeToLower)
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings        (ProxySettings,
                                                                          decProxySettings,
                                                                          encProxySettings)
import           Protocol.Webdriver.ClientAPI.Types.Timeout              (Timeout,
                                                                          decTimeout,
                                                                          encTimeout)

import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Chrome  (ChromeCaps,
                                                                          decChromeCaps,
                                                                          encChromeCaps)
import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox (FirefoxCaps,
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
  deriving (Show, Eq)

decBrowser :: Monad f => D.Decoder f Browser
decBrowser = D.string >>= pure . \case
  "firefox"           -> Firefox
  "chrome"            -> Chrome
  "safari"            -> Safari
  "internet explorer" -> IE
  "opera"             -> Opera
  "phantomjs"         -> PhantomJS
  "iphone"            -> IPhone
  "ipad"              -> IPad
  "android"           -> Android
  "htmlunit"          -> HtmlUnit
  other               -> Browser other

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
  deriving (Show, Eq)

decPageLoad :: Monad f => D.Decoder f PageLoad
decPageLoad = D.string >>= \case
  "none"   -> pure None
  "eager"  -> pure Eager
  "normal" -> pure Normal
  _        -> throwing DE._ConversionFailure "PageLoad"

encPageLoad :: Applicative f => E.Encoder f PageLoad
encPageLoad = encodeToLower show

data Platform
  = Linux
  | Unix
  | Windows
  | Vista
  | XP
  | MacOSX
  | Darwin
  | Platform String
  deriving (Show, Eq)

decPlatform :: Monad f => D.Decoder f Platform
decPlatform = D.string >>= pure . \case
  "windows" -> Windows
  "xp"      -> XP
  "vista"   -> Vista
  "mac"     -> MacOSX
  "darwin"  -> Darwin
  "linux"   -> Linux
  "unix"    -> Unix
  other     -> Platform other

encPlatform :: Applicative f => E.Encoder f Platform
encPlatform = encodeToLower g
  where
    g (Platform p) = p
    g p            = show p

data PromptHandling
  = Dismiss
  | Accept
  | DismissNotify
  | AcceptNotify
  | Ignore
  deriving (Show, Eq)

decPromptHandling :: Monad f => D.Decoder f PromptHandling
decPromptHandling = D.string >>= \case
  "dismiss"            -> pure Dismiss
  "accept"             -> pure Accept
  "dismiss and notify" -> pure DismissNotify
  "accept and notify"  -> pure AcceptNotify
  "ignore"             -> pure Ignore
  _ -> throwing DE._ConversionFailure "PromptHandling"

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

instance EqTag Capability Identity where
  eqTagged BrowserName BrowserName                 = (==)
  eqTagged BrowserVersion BrowserVersion           = (==)
  eqTagged PlatformName PlatformName               = (==)
  eqTagged AcceptInsecureCerts AcceptInsecureCerts = (==)
  eqTagged PageLoadStrategy PageLoadStrategy       = (==)
  eqTagged Proxy Proxy                             = (==)
  eqTagged SetWindowRect SetWindowRect             = (==)
  eqTagged Timeouts Timeouts                       = (==)
  eqTagged StrictFileInteract StrictFileInteract   = (==)
  eqTagged UnhandledBehaviour UnhandledBehaviour   = (==)
  eqTagged FirefoxSettings FirefoxSettings         = (==)
  eqTagged ChromeSettings ChromeSettings           = (==)
  eqTagged _ _                                     = const (const False)

instance ShowTag Capability Identity where
  showTaggedPrec BrowserName         = showsPrec
  showTaggedPrec BrowserVersion      = showsPrec
  showTaggedPrec PlatformName        = showsPrec
  showTaggedPrec AcceptInsecureCerts = showsPrec
  showTaggedPrec PageLoadStrategy    = showsPrec
  showTaggedPrec Proxy               = showsPrec
  showTaggedPrec SetWindowRect       = showsPrec
  showTaggedPrec Timeouts            = showsPrec
  showTaggedPrec StrictFileInteract  = showsPrec
  showTaggedPrec UnhandledBehaviour  = showsPrec
  showTaggedPrec FirefoxSettings     = showsPrec
  showTaggedPrec ChromeSettings      = showsPrec

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
  , D.try (atDM  Proxy                decProxySettings) >>= pure . fromMaybe DM.empty
  , atDM  SetWindowRect        D.bool
  , atDM  Timeouts             decTimeout
  , atDM  StrictFileInteract   D.bool
  , atDM  UnhandledBehaviour   decPromptHandling
  , atDM  FirefoxSettings      decFirefoxCaps
  , atDM  ChromeSettings       decChromeCaps
  ]
  where
    atDM = dmatKey capabilityKeyText

firefox :: Capabilities
firefox = DM.fromList [BrowserName ==> Firefox]

chrome :: Capabilities
chrome = DM.fromList [BrowserName ==> Chrome]
