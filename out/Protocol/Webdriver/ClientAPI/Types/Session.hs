{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Protocol.Webdriver.ClientAPI.Types.Session where

import           Data.List.NonEmpty                              (NonEmpty ((:|)))
import           Data.Text                                       (Text)
import qualified GHC.Generics                                    as GHC
import qualified Waargonaut.Encode                               as E
import           Waargonaut.Generic                              (Generic, HasDatatypeInfo,
                                                                  JsonDecode (..),
                                                                  JsonEncode (..),
                                                                  Options (..),
                                                                  Tagged (..),
                                                                  gDecoder,
                                                                  gEncoder)

import           Protocol.Webdriver.ClientAPI.Types.Capabilities (Capabilities, encodeCapabilities)
import           Protocol.Webdriver.ClientAPI.Types.Internal     (WDJson,
                                                                  trimWaargOpts)

data Capability = Capability
  { _cababilityBrowserName  :: Text
  , _cababilityPlatformName :: Text
  }
  deriving (Show, Eq, GHC.Generic)
instance HasDatatypeInfo Capability
instance Generic Capability

capaOpts :: Options
capaOpts = trimWaargOpts "_cabability"

instance JsonEncode WDJson Capability where mkEncoder = gEncoder capaOpts
instance JsonDecode WDJson Capability where mkDecoder = gDecoder capaOpts

data NewSession = NewSession
  { _newSessionFirstMatch :: NonEmpty Capabilities
  , _newSessionUsername   :: Maybe Text
  , _newSessionPassword   :: Maybe Text
  }

instance JsonEncode WDJson NewSession where
  mkEncoder =
    let
      caps ns = case _newSessionFirstMatch ns of
        o :| [] -> E.atKey' "alwaysMatch" encodeCapabilities o
        xs      -> E.atKey' "firstMatch" (E.nonempty encodeCapabilities) xs

    in
      Tagged $ E.mapLikeObj
        (E.atKey' "capabilities"
          (E.mapLikeObj $ \ns ->
              E.atOptKey' "username" E.text (_newSessionUsername ns) .
              E.atOptKey' "password" E.text (_newSessionPassword ns) .
              caps ns
          )
        )

