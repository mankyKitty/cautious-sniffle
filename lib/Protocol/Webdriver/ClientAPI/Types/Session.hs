{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Constructing a new session or handling the 'SessionId'.
module Protocol.Webdriver.ClientAPI.Types.Session
  ( NewSession (..)
  , SessionId (..)
  , Session (..)
  ) where

import           Data.Text                                       (Text)
import qualified Waargonaut.Decode                               as D
import qualified Waargonaut.Encode                               as E
import           Waargonaut.Generic                              (JsonDecode (..),
                                                                  JsonEncode (..))

import           Servant.API                                     (ToHttpApiData (toUrlPiece))

import           Protocol.Webdriver.ClientAPI.Types.Capabilities (Capabilities, decCapabilities,
                                                                  encCapabilities)
import           Protocol.Webdriver.ClientAPI.Types.Internal     (WDJson)

-- | For creating a new session.
data NewSession = NewSession
  { _newSessionCapabilities :: Capabilities
  , _newSessionUsername     :: Maybe Text
  , _newSessionPassword     :: Maybe Text
  }

-- | Wrapper for the value that represents the current session.
newtype SessionId = SessionId
  { unSessionId :: Text
  }
  deriving (Eq, Show)

instance ToHttpApiData SessionId where
  toUrlPiece (SessionId sid) = sid

-- | Returned upon successful creation of a new session.
data Session = Session
  { _sessionId   :: SessionId
  , _sessionCaps :: Capabilities
  }
  deriving (Show, Eq)

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

