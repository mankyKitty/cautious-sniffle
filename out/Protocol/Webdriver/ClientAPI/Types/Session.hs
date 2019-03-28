{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Protocol.Webdriver.ClientAPI.Types.Session where

import           Data.Text                                       (Text)
import qualified Waargonaut.Decode                               as D
import qualified Waargonaut.Encode                               as E
import           Waargonaut.Generic                              (JsonDecode (..),
                                                                  JsonEncode (..))

import           Servant.API                                     (ToHttpApiData (toUrlPiece))

import           Protocol.Webdriver.ClientAPI.Types.Capabilities (Capabilities, decCapabilities,
                                                                  encCapabilities)
import           Protocol.Webdriver.ClientAPI.Types.Internal     (WDJson)

data NewSession = NewSession
  { _newSessionCapabilities :: Capabilities
  , _newSessionUsername     :: Maybe Text
  , _newSessionPassword     :: Maybe Text
  }

newtype SessionId = SessionId
  { unSessionId :: Text
  }
  deriving (Eq, Show)

instance ToHttpApiData SessionId where
  toUrlPiece (SessionId sid) = sid

data Session = Session
  { _sessionId   :: SessionId
  , _sessionCaps :: Capabilities
  }
  deriving (Show)

instance JsonDecode WDJson Session where
  mkDecoder = pure $ Session
    <$> D.atKey "sessionId" (SessionId <$> D.text)
    <*> D.atKey "capabilities" decCapabilities

instance JsonEncode WDJson NewSession where
  mkEncoder = pure $ E.mapLikeObj
    (E.atKey' "capabilities"
      (E.mapLikeObj $ \ns ->
          E.atOptKey' "username" E.text (_newSessionUsername ns) .
          E.atOptKey' "password" E.text (_newSessionPassword ns) .
          E.atKey' "alwaysMatch" encCapabilities (_newSessionCapabilities ns)
      )
    )

