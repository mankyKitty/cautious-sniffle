{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
module Protocol.Webdriver.ClientAPI.Types.Internal where

import           Data.ByteString            (ByteString)
import           Data.Dependent.Map         (DMap)
import qualified Data.Dependent.Map         as DM
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity      (Identity (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..),
                                             Options (_optionsFieldName),
                                             Tagged (..), defaultOpts,
                                             trimPrefixLowerFirst, untag)
data WDJson

encodeToLower :: Applicative f => (a -> String) -> E.Encoder f a
encodeToLower f = T.toLower . T.pack . f >$< E.text

singleValueObj :: Applicative f => Text -> E.Encoder' a -> E.Encoder f a
singleValueObj k e = E.mapLikeObj $ E.atKey' k e

trimWaargOpts :: Text -> Options
trimWaargOpts s = defaultOpts { _optionsFieldName = trimPrefixLowerFirst s }

encodeDMap
  :: Applicative f
  => (forall v. k v -> (Text, E.Encoder' v))
  -> E.Encoder f (DMap k Identity)
encodeDMap objBits = E.mapLikeObj $ \dm obj -> DM.foldrWithKey
  (\k v -> case (objBits k) of (key, enc) -> E.atKey' key enc (runIdentity v)) obj dm

instance JsonEncode WDJson a => JsonEncode WDJson (Vector a) where
  mkEncoder = E.traversable <$> mkEncoder

instance JsonDecode WDJson a => JsonDecode WDJson (Vector a) where
  mkDecoder = (D.withCursor . D.rightwardSnoc V.empty) <$> mkDecoder

newtype Success a = Success { unSuccess :: a }
  deriving (Show, Eq)

instance JsonDecode WDJson () where
  mkDecoder = Tagged D.null

instance JsonDecode WDJson a => JsonDecode WDJson (Success a) where
  mkDecoder = Tagged $ Success <$> D.atKey "value" (untag $ mkDecoder @WDJson)

newtype Base64 = Base64
  { unBase64 :: ByteString }
  deriving (Show, Eq)

encodeBase64 :: Applicative f => E.Encoder f Base64
encodeBase64 = (TE.decodeUtf8 . unBase64) >$< E.text
