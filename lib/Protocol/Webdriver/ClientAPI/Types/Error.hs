{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.Error 
  ( Error (..)
  , encError
  , decError
  ) where

import           Data.Text                                   (Text)
import           Waargonaut.Generic                          (JsonEncode (..), JsonDecode (..))

import qualified Waargonaut.Encode                           as E
import qualified Waargonaut.Decode                           as D

import           Waargonaut.Types.Json                       (Json)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)

data Error = Error
  { _errorError      :: Text
  , _errorMessage    :: Text
  , _errorStacktrace :: Text
  , _errorData       :: Maybe Json
  }
  deriving (Show, Eq)

encError :: Applicative f => E.Encoder f Error
encError = E.mapLikeObj $ \e ->
  E.atKey' "error" E.text (_errorError e) .
  E.atKey' "message" E.text (_errorMessage e) .
  E.atKey' "stacktrace" E.text (_errorStacktrace e) .
  E.atOptKey' "data" E.json (_errorData e)

decError :: Monad f => D.Decoder f Error
decError = Error
  <$> D.atKey "error" D.text
  <*> D.atKey "message" D.text
  <*> D.atKey "stacktrace" D.text
  <*> D.atKeyOptional "data" D.json

instance JsonDecode WDJson Error where mkDecoder = pure decError
instance JsonEncode WDJson Error where mkEncoder = pure encError