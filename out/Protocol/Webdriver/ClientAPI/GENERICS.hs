{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}

{-# LANGUAGE UndecidableInstances #-}
module Protocol.Webdriver.ClientAPI.GENERICS 
  ( -- * Types
    WebDriverAPI (..)
  , WindowClient
  , ElementClient
  , SessionClient
  , WDApi

    -- * APIs
  , apiProxy
  , windowClient
  , sessionClient
  , elementClient
  , wdClient

    -- * Re-exports
  , module Protocol.Webdriver.ClientAPI.SessionAPI
  ) where

import qualified GHC.Generics                                        as GHC

import           Data.Proxy                                          (Proxy (..))
import           Protocol.Webdriver.ClientAPI.Types.ElementId        (ElementId)
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

import           Protocol.Webdriver.ClientAPI.SessionAPI

import           Servant.API.Generic
import           Servant.Client.Generic                              (AsClientT)

data WebDriverAPI route = WebDriverAPI
  { getStatus   :: route :- "status" :> Get '[WaargJSON WDJson] Json
  , newSession  :: route :- "session" :> ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Value Session)
  , withSession :: route :- "session" :> Capture "sessionId" SessionId :> ToServantApi SessionAPI
  }
  deriving GHC.Generic

apiProxy :: Proxy (ToServantApi WebDriverAPI)
apiProxy = Proxy

type WindowClient = WindowAPI (AsClientT ClientM)
type ElementClient = ElementAPI (AsClientT ClientM)
type SessionClient = SessionAPI (AsClientT ClientM)

type WDApi = WebDriverAPI (AsClientT ClientM)

windowClient :: SessionId -> WindowClient
windowClient sid = fromServant $ withWindow (sessionClient sid)

elementClient :: SessionId -> ElementId -> ElementClient
elementClient sid eid = fromServant $ withElement (sessionClient sid) eid

sessionClient :: SessionId -> SessionClient
sessionClient sessionId = fromServant $ withSession wdClient sessionId

wdClient :: WebDriverAPI (AsClientT ClientM)
wdClient = fromServant $ C.client apiProxy
