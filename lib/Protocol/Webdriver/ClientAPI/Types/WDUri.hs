{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.WDUri where

import           Control.Exception                           (displayException,
                                                              fromException)

import           Data.Bifunctor                              (bimap)
import           Data.Functor.Contravariant                  ((>$<))

import qualified Data.Text                                   as T

import           Text.URI                                    (URI)
import qualified Text.URI                                    as URI

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..))

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson,
                                                              singleValueObj,
                                                              withText)

newtype WDUri = WDUri
  { _unWDUri :: URI }
  deriving (Show, Eq)

encURI :: Applicative f => E.Encoder f WDUri
encURI = singleValueObj "url" (URI.render . _unWDUri >$< E.text)

decURI :: Monad f => D.Decoder f WDUri
decURI = withText (bimap (T.pack . errText . fromException) WDUri . URI.mkURI)
  where
    errText = maybe
      "WDUri : Unknown Error parsing URI"
      (\(URI.ParseException t) -> displayException t)

instance JsonDecode WDJson WDUri where mkDecoder = pure decURI
instance JsonEncode WDJson WDUri where mkEncoder = pure encURI
