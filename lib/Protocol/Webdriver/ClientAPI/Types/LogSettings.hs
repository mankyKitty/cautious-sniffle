{-# LANGUAGE OverloadedStrings #-}
-- | Log settings to describe the required verbosity from the driver.
--
module Protocol.Webdriver.ClientAPI.Types.LogSettings where

import           Control.Error.Util                          (note)
import qualified Data.Char                                   as C
import           Data.Functor.Contravariant                  ((>$<))
import           Text.Read                                   (readMaybe)
import qualified Data.Text as T
import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E

import           Protocol.Webdriver.ClientAPI.Types.Internal (encodeToLower,
                                                              singleValueObj,
                                                              withString)
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
decLogLevel = withString $ \s -> note ("LogLevel : " <> T.pack s) 
  . readMaybe 
  $ C.toUpper <$> s

newtype LogSettings = LogSettings
  { _logLevel :: LogLevel
  }
  deriving (Show, Eq)

decLogSettings :: Monad f => D.Decoder f LogSettings
decLogSettings = LogSettings <$> D.atKey "level" decLogLevel

encLogSettings :: Applicative f => E.Encoder f LogSettings
encLogSettings = _logLevel >$< singleValueObj "level" encLogLevel
