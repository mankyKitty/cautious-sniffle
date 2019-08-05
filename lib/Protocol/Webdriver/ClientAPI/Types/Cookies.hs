{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | A loose representation of a cookie as described in the WebDriver
-- [spec](https://w3c.github.io/webdriver/#cookies)
--
module Protocol.Webdriver.ClientAPI.Types.Cookies
  ( -- * Types
    Cookie
  , HasCookie (..)

    -- * Helpers
  , encCookie
  , decCookie
  ) where

import           Control.Lens                                (makeClassy)
import           Data.Functor.Contravariant                  ((>$<))

import           Data.Text                                   (Text)

import           Data.Time.Clock.POSIX                       (POSIXTime)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)
import           Protocol.Webdriver.ClientAPI.Types.WDUri    (WDUri, decURI,
                                                              encURI)

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..))
import           Waargonaut.Types.Json                       (Json)

import           Generics.SOP.TH                             (deriveGeneric)

-- | A cookie, singular, as indicated by the name, which is not plural.
data Cookie = Cookie
  { _cookieName       :: Text
  , _cookieValue      :: Json
  , _cookiePath       :: Maybe WDUri
  , _cookieDomain     :: Maybe Text
  , _cookieSecureOnly :: Maybe Bool
  , _cookieHttpOnly   :: Maybe Bool
  , _cookieExpiryTime :: Maybe POSIXTime
  }
  deriving (Eq, Show)
makeClassy ''Cookie
deriveGeneric ''Cookie

decCookie :: Monad f => D.Decoder f Cookie
decCookie = Cookie
  <$> D.atKey "name" D.text
  <*> D.atKey "value" D.json
  <*> D.atKeyOptional "path" decURI
  <*> D.atKeyOptional "domain" D.text
  <*> D.atKeyOptional "secure" D.bool
  <*> D.atKeyOptional "httpOnly" D.bool
  <*> D.atKeyOptional "expiry" (fromInteger . fromIntegral <$> D.int)

encCookie :: Applicative f => E.Encoder f Cookie
encCookie = E.mapLikeObj $ \c ->
  E.atKey' "name" E.text (_cookieName c) .
  E.atKey' "value" E.json (_cookieValue c) .
  E.atOptKey' "path" encURI (_cookiePath c) .
  E.atOptKey' "domain" E.text (_cookieDomain c) .
  E.atOptKey' "secure" E.bool (_cookieSecureOnly c) .
  E.atOptKey' "httpOnly" E.bool (_cookieHttpOnly c) .
  E.atOptKey' "expiry" (floor >$< E.int) (_cookieExpiryTime c)

instance JsonDecode WDJson Cookie where mkDecoder = pure decCookie
instance JsonEncode WDJson Cookie where mkEncoder = pure encCookie
