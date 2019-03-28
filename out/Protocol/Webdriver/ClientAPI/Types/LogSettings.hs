{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.ClientAPI.Types.LogSettings where

import           Control.Monad.Error.Lens                    (throwing)
import qualified Data.Char                                   as C
import           Data.Functor.Contravariant                  ((>$<))
import           Text.Read                                   (readMaybe)

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Decode.Error                     as DE
import qualified Waargonaut.Encode                           as E

import           Protocol.Webdriver.ClientAPI.Types.Internal (encodeToLower,
                                                              singleValueObj)
data LogLevel
  = TRACE
  | DEBUG
  | CONFIG
  | INFO
  | WARN
  | ERROR
  | FATAL
  deriving (Read, Show, Eq)

encLogLevel :: Applicative f => E.Encoder f LogLevel
encLogLevel = encodeToLower show

decLogLevel :: Monad f => D.Decoder f LogLevel
decLogLevel = D.string >>= maybe (throwing DE._ConversionFailure "LogLevel") pure . mll
  where
    mll :: String -> Maybe LogLevel
    mll = readMaybe . fmap C.toUpper

data LogSettings = LogSettings
  { _logLevel :: LogLevel
  }
  deriving (Show, Eq)

decLogSettings :: Monad f => D.Decoder f LogSettings
decLogSettings = LogSettings <$> D.atKey "level" decLogLevel

encLogSettings :: Applicative f => E.Encoder f LogSettings
encLogSettings = _logLevel >$< singleValueObj "level" encLogLevel
