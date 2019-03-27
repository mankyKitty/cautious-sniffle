{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Protocol.Webdriver.ClientAPI.Types.ProxySettings where

import           Data.Functor.Contravariant                  ((>$<))
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           GHC.Word                                    (Word16, Word8)

import           Data.Dependent.Map                          (DMap, fromList)
import qualified Data.Dependent.Map                          as DM
import           Data.Dependent.Sum                          ((==>))
import           Data.Foldable                               (fold)
import           Data.Functor.Identity                       (Identity (..))

import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH

import qualified Waargonaut.Encode                           as E

import           Protocol.Webdriver.ClientAPI.Types.Internal (encodeToLower)

data ProxyType
  = Pac        -- "pac"
  | Direct     -- "direct"
  | Autodetect -- "autodetect"
  | System     -- "system"
  | Manual     -- "manual".
  deriving (Show, Eq)

encodeProxyType :: Applicative f => E.Encoder f ProxyType
encodeProxyType = encodeToLower show

newtype HostPort = HostPort (Text, Maybe Word16)
  deriving (Show, Eq)

encodeHostPort :: Applicative f => E.Encoder f HostPort
encodeHostPort = f >$< E.text where
  f (HostPort (host, p)) = host <> ":" <> T.pack (fold (show <$> p))

newtype SocksVersion = SocksVersion { unSocksVersion :: Word8 }
  deriving (Show, Eq)

encodeSocksVersion :: Applicative f => E.Encoder f SocksVersion
encodeSocksVersion = unSocksVersion >$< E.integral

data ProxySetting a where
  PType         :: ProxySetting ProxyType
  AutoconfigUrl :: ProxySetting Text
  Ftp           :: ProxySetting HostPort
  Http          :: ProxySetting HostPort
  NoProxy       :: ProxySetting [Text]
  Https         :: ProxySetting HostPort
  Socks         :: ProxySetting HostPort
  SocksVer      :: ProxySetting SocksVersion

proxySettingKey :: ProxySetting a -> Text
proxySettingKey PType         = "proxyType"
proxySettingKey AutoconfigUrl = "proxyAutoconfigUrl"
proxySettingKey Ftp           = "ftpProxy"
proxySettingKey Http          = "httpProxy"
proxySettingKey NoProxy       = "noProxy"
proxySettingKey Https         = "sslProxy"
proxySettingKey Socks         = "socksProxy"
proxySettingKey SocksVer      = "socksVersion"

proxySettingEncode :: Applicative f => ProxySetting a -> E.Encoder f a
proxySettingEncode PType         = encodeProxyType
proxySettingEncode AutoconfigUrl = E.text
proxySettingEncode Ftp           = encodeHostPort
proxySettingEncode Http          = encodeHostPort
proxySettingEncode NoProxy       = E.list E.text
proxySettingEncode Https         = encodeHostPort
proxySettingEncode Socks         = encodeHostPort
proxySettingEncode SocksVer      = encodeSocksVersion

deriving instance Eq (ProxySetting a)
deriving instance Ord (ProxySetting a)
deriving instance Show (ProxySetting a)

deriveGShow ''ProxySetting
deriveGEq ''ProxySetting
deriveGCompare ''ProxySetting

type ProxySettings = DMap ProxySetting Identity

encodeProxySettings :: Applicative f => E.Encoder f ProxySettings
encodeProxySettings = E.mapLikeObj $ \ps obj -> DM.foldrWithKey
  (\k (Identity v) -> E.atKey' (proxySettingKey k) (proxySettingEncode k) v) obj ps

emptyManualProxy :: ProxySettings
emptyManualProxy = fromList [PType ==> Manual]

ftpProxy :: HostPort -> ProxySettings
ftpProxy hp = DM.insert Ftp (pure hp) emptyManualProxy
