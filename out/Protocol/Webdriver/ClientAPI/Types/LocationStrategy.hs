{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.LocationStrategy where

import           Data.Text                                   (Text)
import qualified GHC.Generics                                as GHC
import           Waargonaut.Generic                          (Generic,
                                                              HasDatatypeInfo,
                                                              JsonDecode (..),
                                                              JsonEncode (..),
                                                              gDecoder,
                                                              gEncoder)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson, trimWaargOpts)

data Locate = Locate
  { _locateUsing :: Text
  , _locateValue :: Text
  }
  deriving (Show, Eq, GHC.Generic)

instance HasDatatypeInfo Locate
instance Generic Locate

instance JsonEncode WDJson Locate where
  mkEncoder = gEncoder (trimWaargOpts "_locate")

instance JsonDecode WDJson Locate where
  mkDecoder = gDecoder (trimWaargOpts "_locate")
