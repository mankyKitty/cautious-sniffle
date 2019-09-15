{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Wrapper of the 'URI' type from the [modern-uri](https://hackage.haskell.org/package/modern-uri) package.
module Protocol.Webdriver.ClientAPI.Types.WDUri
  ( WDUri (..)
  , encURI
  , decURI
  , module Text.URI
  , module Text.URI.QQ
  ) where

import           Control.Exception                           (displayException,
                                                              fromException)

import           Data.Bifunctor                              (bimap)
import           Data.Functor.Contravariant                  ((>$<))

import qualified Data.Text                                   as T

import           Text.URI                                    
import           Text.URI.QQ                                  

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
encURI = singleValueObj "url" (render . _unWDUri >$< E.text)

decURI :: Monad f => D.Decoder f WDUri
decURI = withText (bimap (T.pack . errText . fromException) WDUri . mkURI)
  where
    errText = maybe
      "WDUri : Unknown Error parsing URI"
      (\(ParseException t) -> displayException t)

instance JsonDecode WDJson WDUri where mkDecoder = pure decURI
instance JsonEncode WDJson WDUri where mkEncoder = pure encURI
