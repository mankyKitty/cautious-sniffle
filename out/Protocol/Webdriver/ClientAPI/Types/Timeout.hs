{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.Timeout where

import           Data.Functor.Contravariant                  ((>$<))
import           GHC.Word                                    (Word32)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)
import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..))

newtype TimeoutVal = TimeoutVal
  { unTimeoutVal :: Word32 }
  deriving (Show, Eq)

decTimeoutVal :: Monad f => D.Decoder f TimeoutVal
decTimeoutVal = TimeoutVal <$> D.integral

encTimeoutVal :: Applicative f => E.Encoder f TimeoutVal
encTimeoutVal = unTimeoutVal >$< E.integral

data Timeout = Timeout
  { _timeoutScript       :: TimeoutVal
  , _timeoutPageLoad     :: TimeoutVal
  , _timeoutImplicitWait :: TimeoutVal
  }
  deriving (Show, Eq)

decTimeout :: Monad f => D.Decoder f Timeout
decTimeout = Timeout
  <$> D.atKey "script" decTimeoutVal
  <*> D.atKey "pageLoad" decTimeoutVal
  <*> D.atKey "implicit" decTimeoutVal

defaultTimeout :: Timeout
defaultTimeout = Timeout (TimeoutVal 30000) (TimeoutVal 300000) (TimeoutVal 0)

encTimeout :: Applicative f => E.Encoder f Timeout
encTimeout = E.mapLikeObj $ \to ->
  E.atKey' "script" encTimeoutVal (_timeoutScript to) .
  E.atKey' "pageLoad" encTimeoutVal (_timeoutPageLoad to) .
  E.atKey' "implicit" encTimeoutVal (_timeoutImplicitWait to)

instance JsonDecode WDJson Timeout where mkDecoder = pure decTimeout
instance JsonEncode WDJson Timeout where mkEncoder = pure encTimeout
