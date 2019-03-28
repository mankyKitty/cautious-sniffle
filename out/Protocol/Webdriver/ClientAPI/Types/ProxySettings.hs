{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module Protocol.Webdriver.ClientAPI.Types.ProxySettings where

import           Control.Monad.Error.Lens                    (throwing)
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           GHC.Word                                    (Word16, Word8)

import           Data.Dependent.Map                          (DMap, fromList)
import qualified Data.Dependent.Map                          as DM
import           Data.Dependent.Sum                          (ShowTag (..), EqTag (..),
                                                              (==>))
import           Data.Foldable                               (fold)
import           Data.Functor.Identity                       (Identity (..))

import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH

import           Text.Read                                   (readMaybe)

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Decode.Error                     as DE
import qualified Waargonaut.Encode                           as E

import           Protocol.Webdriver.ClientAPI.Types.Internal (decodeDMap,
                                                              dmatKey,
                                                              encodeToLower)

data ProxyType
  = Pac        -- "pac"
  | Direct     -- "direct"
  | Autodetect -- "autodetect"
  | System     -- "system"
  | Manual     -- "manual".
  deriving (Show, Eq)

decProxyType :: Monad f => D.Decoder f ProxyType
decProxyType = D.string >>= \case
 "pac"        -> pure Pac
 "direct"     -> pure Direct
 "autodetect" -> pure Autodetect
 "system"     -> pure System
 "manual"     -> pure Manual
 _            -> throwing DE._ConversionFailure "ProxyType"

encProxyType :: Applicative f => E.Encoder f ProxyType
encProxyType = encodeToLower show

newtype HostPort = HostPort (Text, Maybe Word16)
  deriving (Show, Eq)

decHostPort :: Monad f => D.Decoder f HostPort
decHostPort = D.text >>= \t -> case T.breakOn ":" t of
  (host, port) -> pure $ HostPort (host, readMaybe (T.unpack port))

encHostPort :: Applicative f => E.Encoder f HostPort
encHostPort = f >$< E.text where
  f (HostPort (host, p)) = host <> ":" <> T.pack (fold (show <$> p))

newtype SocksVersion = SocksVersion { unSocksVersion :: Word8 }
  deriving (Show, Eq)

decSocksVersion :: Monad f => D.Decoder f SocksVersion
decSocksVersion = SocksVersion <$> D.integral

encSocksVersion :: Applicative f => E.Encoder f SocksVersion
encSocksVersion = unSocksVersion >$< E.integral

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
proxySettingEncode PType         = encProxyType
proxySettingEncode AutoconfigUrl = E.text
proxySettingEncode Ftp           = encHostPort
proxySettingEncode Http          = encHostPort
proxySettingEncode NoProxy       = E.list E.text
proxySettingEncode Https         = encHostPort
proxySettingEncode Socks         = encHostPort
proxySettingEncode SocksVer      = encSocksVersion

deriving instance Eq (ProxySetting a)
deriving instance Ord (ProxySetting a)
deriving instance Show (ProxySetting a)

instance EqTag ProxySetting Identity where
  eqTagged PType PType                 = (==)
  eqTagged AutoconfigUrl AutoconfigUrl = (==)
  eqTagged Ftp Ftp                     = (==)
  eqTagged Http Http                   = (==)
  eqTagged NoProxy NoProxy             = (==)
  eqTagged Https Https                 = (==)
  eqTagged Socks Socks                 = (==)
  eqTagged SocksVer SocksVer           = (==)
  eqTagged _ _                         = const (const False)

instance ShowTag ProxySetting Identity where
  showTaggedPrec PType         = showsPrec
  showTaggedPrec AutoconfigUrl = showsPrec
  showTaggedPrec Ftp           = showsPrec
  showTaggedPrec Http          = showsPrec
  showTaggedPrec NoProxy       = showsPrec
  showTaggedPrec Https         = showsPrec
  showTaggedPrec Socks         = showsPrec
  showTaggedPrec SocksVer      = showsPrec

deriveGShow ''ProxySetting
deriveGEq ''ProxySetting
deriveGCompare ''ProxySetting

type ProxySettings = DMap ProxySetting Identity

decProxySettings :: Monad f => D.Decoder f ProxySettings
decProxySettings = decodeDMap
  [ atDM PType decProxyType
  , atDM AutoconfigUrl D.text
  , atDM Ftp decHostPort
  , atDM Http decHostPort
  , atDM NoProxy (D.list D.text)
  , atDM Https decHostPort
  , atDM Socks decHostPort
  , atDM SocksVer decSocksVersion
  ]
  where
    atDM = dmatKey proxySettingKey

encProxySettings :: Applicative f => E.Encoder f ProxySettings
encProxySettings = E.mapLikeObj $ \ps obj -> DM.foldrWithKey
  (\k (Identity v) -> E.atKey' (proxySettingKey k) (proxySettingEncode k) v) obj ps

emptyManualProxy :: ProxySettings
emptyManualProxy = fromList [PType ==> Manual]

ftpProxy :: HostPort -> ProxySettings
ftpProxy hp = DM.insert Ftp (pure hp) emptyManualProxy
