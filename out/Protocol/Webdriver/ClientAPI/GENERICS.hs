{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}

{-# LANGUAGE UndecidableInstances #-}
module Protocol.Webdriver.ClientAPI.GENERICS 
  ( -- * Types
    WebDriverAPI (..)
  , UsingSess
  , WdApi

    -- * APIs
  , apiProxy
  , withSessionClient
  , wdClient

    -- * Re-exports
  , module Protocol.Webdriver.ClientAPI.WithSessionAPI
  ) where

import qualified GHC.Generics                                        as GHC

import           Data.Proxy                                          (Proxy (..))
import           Protocol.Webdriver.ClientAPI.Types.Internal         (Value,
                                                                      WDJson)
import           Protocol.Webdriver.ClientAPI.Types.Session          (NewSession,
                                                                      Session,
                                                                      SessionId)

import           Servant.API
import           Servant.API.ContentTypes.Waargonaut                 (WaargJSON)

import           Servant.Client                                      (ClientM)
import qualified Servant.Client                                      as C

import           Waargonaut.Types.Json                               (Json)

import           Protocol.Webdriver.ClientAPI.WithSessionAPI

import           Servant.API.Generic
import           Servant.Client.Generic                              (AsClientT)

data WebDriverAPI route = WebDriverAPI
  { getStatus   :: route :- "status" :> Get '[WaargJSON WDJson] Json
  , newSession  :: route :- "session" :> ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Value Session)
  , withSession :: route :- "session" :> Capture "sessionId" SessionId :> ToServantApi WithSessionAPI
  }
  deriving GHC.Generic

apiProxy :: Proxy (ToServantApi WebDriverAPI)
apiProxy = Proxy

type UsingSess = WithSessionAPI (AsClientT ClientM)
type WdApi = WebDriverAPI (AsClientT ClientM)

withSessionClient :: SessionId -> WithSessionAPI (AsClientT ClientM)
withSessionClient sessionId = fromServant $ withSession wdClient sessionId

wdClient :: WebDriverAPI (AsClientT ClientM)
wdClient = fromServant $ C.client apiProxy
