{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.ClientAPI.Types.LogSettings where

import           Data.Functor.Contravariant                  ((>$<))

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
  deriving (Show, Eq)

encodeLogLevel :: Applicative f => E.Encoder f LogLevel
encodeLogLevel = encodeToLower show

data LogSettings = LogSettings
  { _logLevel :: LogLevel
  }
  deriving (Show, Eq)

encodeLogSettings :: Applicative f => E.Encoder f LogSettings
encodeLogSettings = _logLevel >$< singleValueObj "level" encodeLogLevel
