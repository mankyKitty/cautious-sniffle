{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Protocol.Webdriver.ClientAPI
  ( -- * Types
    WebDriverAPI (..)

    -- * APIs
  , apiProxy
  , windowClient
  , sessionClient
  , elementClient
  , wdClient

    -- * Helpers
  , defaultWebdriverClient

    -- * Re-exports
  , module Protocol.Webdriver.ClientAPI.SessionAPI
  ) where

import qualified GHC.Generics                            as GHC

import           Data.Proxy                              (Proxy (..))
import           Protocol.Webdriver.ClientAPI.Types      (ElementId, NewSession,
                                                          Session, SessionId,
                                                          Success, WDJson)

import           Servant.API
import           Servant.API.ContentTypes.Waargonaut     (WaargJSON)

import           Servant.Client                          (BaseUrl, ClientM)
import qualified Servant.Client                          as C

import           Waargonaut.Types.Json                   (Json)

import           Protocol.Webdriver.ClientAPI.SessionAPI

import           Servant.API.Generic
import           Servant.Client.Generic                  (AsClientT)

data WebDriverAPI route = WebDriverAPI
  { getStatus   :: route :- "status" :> Get '[WaargJSON WDJson] Json
  , newSession  :: route :- "session" :> ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Success Session)
  , withSession :: route :- "session" :> Capture "sessionId" SessionId :> ToServantApi SessionAPI
  }
  deriving GHC.Generic

apiProxy :: Proxy (ToServantApi WebDriverAPI)
apiProxy = genericApi (Proxy @WebDriverAPI)

windowClient :: SessionId -> WindowAPI (AsClientT ClientM)
windowClient sid = fromServant $ withWindow (sessionClient sid)

elementClient :: SessionId -> ElementId -> ElementAPI (AsClientT ClientM)
elementClient sid eid = fromServant $ withElement (sessionClient sid) eid

sessionClient :: SessionId -> SessionAPI (AsClientT ClientM)
sessionClient sessionId = fromServant $ withSession wdClient sessionId

wdClient :: WebDriverAPI (AsClientT ClientM)
wdClient = fromServant $ C.client apiProxy

defaultWebdriverClient :: BaseUrl
defaultWebdriverClient = C.BaseUrl C.Http "localhost" 4444 "/wd/hub"
