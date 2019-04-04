{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
module Protocol.Webdriver.ClientAPI.Types.Internal where

import           Control.Error.Util         (note)
import           Control.Lens               (over, (%~), (?~), _head)
import           Control.Monad.Error.Lens   (throwing)

import           Data.Bifunctor             (bimap)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as B8

import qualified Data.Char                  as C

import           Data.Dependent.Map         (DMap, GCompare)
import qualified Data.Dependent.Map         as DM
import           Data.Dependent.Map.Lens    (dmat)

import           Data.Function              ((&))
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity      (Identity (..))

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Text.Read                  (readMaybe)

import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as DE
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..),
                                             Options (_optionsFieldName),
                                             defaultOpts, trimPrefixLowerFirst,
                                             untag)
data WDJson

withA :: Monad f => D.Decoder f a -> (a -> Either Text b) -> D.Decoder f b
withA from to = from >>= either (throwing DE._ConversionFailure) pure . to

withString :: Monad f => (String -> Either Text a) -> D.Decoder f a
withString = withA D.string

withText :: Monad f => (Text -> Either Text a) -> D.Decoder f a
withText = withA D.text

encodeShowToLower :: (Show a, Applicative f) => E.Encoder f a
encodeShowToLower = T.toLower . T.pack . show >$< E.text

decodeFromReadUCFirst :: (Read a, Monad f) => Text -> D.Decoder f a
decodeFromReadUCFirst l = withString (note l . readMaybe . over _head C.toUpper)

encodeToLower :: Applicative f => (a -> String) -> E.Encoder f a
encodeToLower f = T.toLower . T.pack . f >$< E.text

singleValueObj :: Applicative f => Text -> E.Encoder' a -> E.Encoder f a
singleValueObj k = E.mapLikeObj . E.atKey' k

trimWaargOpts :: Text -> Options
trimWaargOpts s = defaultOpts { _optionsFieldName = trimPrefixLowerFirst s }

infixr 2 ~=>

(~=>) :: (GCompare k, Applicative f) => k a -> a -> DMap k f -> DMap k f
(~=>) k v = dmat k ?~ pure v

updateInnerSetting
  :: ( Applicative g
     , Applicative f
     , DM.GCompare k1
     , DM.GCompare k2
     )
  => k1 (DMap k2 f)
  -> k2 a
  -> a
  -> (a -> a)
  -> DMap k1 g
  -> DMap k1 g
updateInnerSetting outerKey innerKey opt f dm0 = dm0 & dmat outerKey %~ Just . maybe
  (pure $ DM.singleton innerKey (pure opt))
  (fmap (dmat innerKey %~ Just . maybe (pure opt) (fmap f)))

dmatKey
  :: ( GCompare k
     , Monad g
     , Applicative f
     )
  => (k a -> Text)
  -> k a
  -> D.Decoder g a
  -> D.Decoder g (DMap k f)
dmatKey toText k d = D.atKeyOptional (toText k) d >>= pure . \case
  Nothing -> DM.empty
  Just v  -> DM.singleton k (pure v)

decodeDMap
  :: ( GCompare k
     , Monad f
     , Functor g
     )
  => [D.Decoder f (DMap k g)]
  -> D.Decoder f (DMap k g)
decodeDMap =
  fmap DM.unions . sequenceA

encodeDMap
  :: Applicative f
  => (forall v. k v -> (Text, E.Encoder' v))
  -> E.Encoder f (DMap k Identity)
encodeDMap objBits = E.mapLikeObj $ \dm obj -> DM.foldrWithKey
  (\k v -> case (objBits k) of (key, enc) -> E.atKey' key enc (runIdentity v)) obj dm

instance JsonEncode WDJson a => JsonEncode WDJson (Vector a) where
  mkEncoder = E.traversable <$> mkEncoder

instance JsonDecode WDJson a => JsonDecode WDJson (Vector a) where
  mkDecoder = D.withCursor . D.rightwardSnoc V.empty <$> mkDecoder

newtype Value a = Value { unValue :: a }
  deriving (Show, Eq)

instance JsonDecode WDJson () where
  mkDecoder = pure D.null

instance JsonDecode WDJson a => JsonDecode WDJson (Value a) where
  mkDecoder = pure $ Value <$> D.atKey "value" (untag $ mkDecoder @WDJson)

newtype Base64 = Base64
  { unBase64 :: ByteString }
  deriving (Show, Eq)

decBase64 :: Monad f => D.Decoder f Base64
decBase64 = withString $ (bimap (mappend "Base64 : " . T.pack) Base64 . B64.decode . B8.pack)

encBase64 :: Applicative f => E.Encoder f Base64
encBase64 = (B8.unpack . B64.encode . unBase64) >$< E.string

instance JsonDecode WDJson Base64 where mkDecoder = pure decBase64
instance JsonEncode WDJson Base64 where mkEncoder = pure encBase64