{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Protocol.Webdriver.ClientAPI.Types.Cookies
  ( Cookie
  , CookieKey (..)
  , encCookie
  , decCookie
  , cookieKeys
  , cookieKeyEnc
  ) where

import           Control.Arrow                               ((&&&))
import           Data.Function                               ((&))
import           Data.Functor                                ((<&>))
import           Data.Functor.Contravariant                  ((>$<))

import           Data.Text                                   (Text)

import           Data.Dependent.Map                          (DMap)
import           Data.Functor.Identity                       (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH

import           Data.Time.Clock.POSIX                       (POSIXTime)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson,
                                                              decodeDMap,
                                                              dmatKey,
                                                              encodeDMap, (~=>))

import           Protocol.Webdriver.ClientAPI.Types.WDUri    (WDUri, decURI,
                                                              encURI)

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..))
import           Waargonaut.Types.Json                       (Json)

data CookieKey v where
  CookieName       :: CookieKey Text
  CookieValue      :: CookieKey Json
  CookiePath       :: CookieKey WDUri
  CookieDomain     :: CookieKey Text
  CookieSecureOnly :: CookieKey Bool
  CookieHttpOnly   :: CookieKey Bool
  CookieExpiryTime :: CookieKey POSIXTime

deriveGShow ''CookieKey
deriveGEq ''CookieKey
deriveGCompare ''CookieKey
deriveEqTagIdentity ''CookieKey
deriveShowTagIdentity ''CookieKey

type Cookie = DMap CookieKey Identity

cookieKeys :: CookieKey v -> Text
cookieKeys CookieName       = "name"
cookieKeys CookieValue      = "value"
cookieKeys CookiePath       = "path"
cookieKeys CookieDomain     = "domain"
cookieKeys CookieSecureOnly = "secure"
cookieKeys CookieHttpOnly   = "httpOnly"
cookieKeys CookieExpiryTime = "expiry"

cookieKeyEnc :: Applicative f => CookieKey v -> E.Encoder f v
cookieKeyEnc CookieName       = E.text
cookieKeyEnc CookieValue      = E.json
cookieKeyEnc CookiePath       = encURI
cookieKeyEnc CookieDomain     = E.text
cookieKeyEnc CookieSecureOnly = E.bool
cookieKeyEnc CookieHttpOnly   = E.bool
cookieKeyEnc CookieExpiryTime = floor >$< E.int

decCookie :: Monad f => D.Decoder f Cookie
decCookie = D.withCursor $ \c -> do
  nom <- D.fromKey "name" D.text c
  val <- D.fromKey "value" D.json c
  D.focus optionals c <&> \rest -> rest
    & CookieName ~=> nom
    & CookieValue ~=> val
  where
    ak = dmatKey cookieKeys
    optionals = decodeDMap
      [ ak CookiePath decURI
      , ak CookieDomain D.text
      , ak CookieSecureOnly D.bool
      , ak CookieHttpOnly D.bool
      , ak CookieExpiryTime (fromInteger . fromIntegral <$> D.int)
      ]

encCookie :: Applicative f => E.Encoder f Cookie
encCookie = encodeDMap (cookieKeys &&& cookieKeyEnc)

instance JsonDecode WDJson (DMap CookieKey Identity) where mkDecoder = pure decCookie
instance JsonEncode WDJson (DMap CookieKey Identity) where mkEncoder = pure encCookie
