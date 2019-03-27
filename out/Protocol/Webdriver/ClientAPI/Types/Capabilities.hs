{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities where

import           Data.Functor.Contravariant                              ((>$<))
import           Data.Text                                               (Text)
import           GHC.Word                                                (Word32)

import           Data.Dependent.Map                                      (DMap)
import qualified Data.Dependent.Map                                      as DM
import           Data.Dependent.Sum                                      ((==>))
import           Data.Functor.Identity                                   (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH

import qualified Waargonaut.Encode                                       as E

import           Protocol.Webdriver.ClientAPI.Types.Internal             (encodeDMap,
                                                                          encodeToLower)
import           Protocol.Webdriver.ClientAPI.Types.ProxySettings        (ProxySettings,
                                                                          encodeProxySettings)

import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Chrome  (ChromeCaps, encodeChromeCaps)
import           Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox (FirefoxCaps, encodeFirefoxCaps)

data Browser
  = Firefox
  | Chrome
  | Safari
  | IE
  | PhantomJS
  | Browser String
  deriving (Show, Eq)

encodeBrowser :: Applicative f => E.Encoder f Browser
encodeBrowser = encodeToLower floop
  where
    floop (Browser b) = b
    floop b           = show b

newtype BrowserVersionString = BrowserVersionString
  { unBrowserVersionString :: Text }
  deriving (Show, Eq)

data PageLoad
  = None
  | Eager
  | Normal
  deriving (Show, Eq)

encodePageLoad :: Applicative f => E.Encoder f PageLoad
encodePageLoad = encodeToLower show

newtype TimeoutVal = TimeoutVal
  { unTimeoutVal :: Word32 }
  deriving (Show, Eq)

encodeTimeoutVal :: Applicative f => E.Encoder f TimeoutVal
encodeTimeoutVal = unTimeoutVal >$< E.integral

data TimeoutValErr
  = TimeoutLessThanZero
  | TimeoutExceeds2to16
  deriving (Show, Eq)

mkTimeoutVal :: Integer -> Either TimeoutValErr TimeoutVal
mkTimeoutVal n
  | n < 0        = Left TimeoutLessThanZero
  | n > maxValue = Left TimeoutExceeds2to16
  | otherwise    = Right (TimeoutVal (fromIntegral n))
  where
    maxValue :: Integer -- SERIOUSLY!?
    maxValue = ((2::Integer)^(16::Integer)) - 1

data Timeout = Timeout
  { _timeoutScript       :: TimeoutVal
  , _timeoutPageLoad     :: TimeoutVal
  , _timeoutImplicitWait :: TimeoutVal
  }
  deriving (Show, Eq)

defaultTimeout :: Timeout
defaultTimeout = Timeout (TimeoutVal 30000) (TimeoutVal 300000) (TimeoutVal 0)

encodeTimeout :: Applicative f => E.Encoder f Timeout
encodeTimeout = E.mapLikeObj $ \to ->
  E.atKey' "script" encodeTimeoutVal (_timeoutScript to) .
  E.atKey' "pageLoad" encodeTimeoutVal (_timeoutPageLoad to) .
  E.atKey' "implicit" encodeTimeoutVal (_timeoutImplicitWait to)

data Platform
  = Linux
  | Windows
  | OSX
  | Platform String
  deriving (Show, Eq)

encodePlatform :: Applicative f => E.Encoder f Platform
encodePlatform = encodeToLower g
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

encodePromptHandling :: Applicative f => E.Encoder f PromptHandling
encodePromptHandling = g >$< E.text
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

deriving instance Eq (Capability a)
deriving instance Ord (Capability a)
deriving instance Show (Capability a)

deriveGShow ''Capability
deriveGEq ''Capability
deriveGCompare ''Capability

type Capabilities = DMap Capability Identity

encodeCapabilities :: Applicative f => E.Encoder f Capabilities
encodeCapabilities = encodeDMap $ \k -> (capabilityKeyText k, encodeAtKey k)
  where
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
      StrictFileInteract  -> "strictFileInteactability"
      UnhandledBehaviour  -> "unhandledPromptBehavior"
      FirefoxSettings     -> "moz:firefoxOptions"
      ChromeSettings      -> "goog:chromeOptions"

    encodeAtKey :: Applicative f => Capability a -> E.Encoder f a
    encodeAtKey k = case k of
      BrowserName         -> encodeBrowser
      BrowserVersion      -> unBrowserVersionString >$< E.text
      PlatformName        -> encodePlatform
      AcceptInsecureCerts -> E.bool
      PageLoadStrategy    -> encodePageLoad
      Proxy               -> encodeProxySettings
      SetWindowRect       -> E.bool
      Timeouts            -> encodeTimeout
      StrictFileInteract  -> E.bool
      UnhandledBehaviour  -> encodePromptHandling
      FirefoxSettings     -> encodeFirefoxCaps
      ChromeSettings      -> encodeChromeCaps

emptyCapabilities :: Capabilities
emptyCapabilities = DM.empty

useFirefoxOn :: Platform -> Capabilities
useFirefoxOn p = DM.fromList [BrowserName ==> Firefox, PlatformName ==> p]

useChromeOn :: Platform -> Capabilities
useChromeOn p = DM.fromList [BrowserName ==> Chrome, PlatformName ==> p]
