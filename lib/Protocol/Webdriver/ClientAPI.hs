{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Contains the root WebDriverAPI, as well as helper structures like 'WDCore' to help
-- with interacting with the rest of the API.
module Protocol.Webdriver.ClientAPI
  ( -- * Types
    WebDriverAPI (..)
  , WDCore (..)
  , HasWDCore (..) 

    -- * APIs
  , apiProxy
  , windowClient
  , sessionClient
  , elementClient
  , wdClient

    -- * Helpers
  , defaultWebdriverClient
  , mkWDCoreClientM
  , mkWDCoreHoistedClient
  , mkWDCoreComposeTClient
  , mkWDCoreExceptTClient

    -- * Re-exports
  , module Protocol.Webdriver.ClientAPI.SessionAPI
  ) where

import           Control.Lens                            (makeClassy)

import Control.Monad.Except (ExceptT (..))
import Data.Functor.Compose (Compose (..))

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
import           Servant.Client.Generic                  (AsClientT, genericClientHoist)

-- | The root API record for interacting with the Webdriver.
data WebDriverAPI route = WebDriverAPI
  { -- | Try to retrieve information about the current remote end.
    getStatus   :: route :- "status" :> Get '[WaargJSON WDJson] Json
    -- | Start a new webdriver 'Session' from the given capabilities in 'NewSession'.
  , newSession  :: route :- "session" :> ReqBody '[WaargJSON WDJson] NewSession :> Post '[WaargJSON WDJson] (Success Session)
    -- | Retrieve the record of functions for interacting with the
    -- given 'SessionId', all commands in this record will be for that
    -- session only.
  , withSession :: route :- "session" :> Capture "sessionId" SessionId :> ToServantApi SessionAPI
  }
  deriving GHC.Generic

-- | Generic 'Proxy' for the 'WebDriverAPI'
apiProxy :: Proxy (ToServantApi WebDriverAPI)
apiProxy = genericApi (Proxy @WebDriverAPI)

-- | For the given 'SessionId' return a record of commands for
-- interacting with the 'WindowAPI' for that session.
--
windowClient :: SessionId -> WindowAPI (AsClientT ClientM)
windowClient sid = fromServant $ withWindow (sessionClient sid)

-- | For the given 'ElementId' in the provided 'SessionId' provide a
-- record of commands for interacting with that element.
--
elementClient :: SessionId -> ElementId -> ElementAPI (AsClientT ClientM)
elementClient sid eid = fromServant $ withElement (sessionClient sid) eid

-- | For the given 'SessionId', create a record of commands for
-- interacting with the browser.
sessionClient :: SessionId -> SessionAPI (AsClientT ClientM)
sessionClient sessionId = fromServant $ withSession wdClient sessionId

-- | Root client API record.
wdClient :: WebDriverAPI (AsClientT ClientM)
wdClient = fromServant $ C.client apiProxy

-- | Defaults for interacting with a webdriver running at @http://localhost:4444/wd/hub@
defaultWebdriverClient :: BaseUrl
defaultWebdriverClient = C.BaseUrl C.Http "localhost" 4444 "/wd/hub"

-- | This is a helper structure that makes it easier to create and use
-- the different subsets of webdriver commands.
--
-- Create a 'WDCore' using one of the provided functions:
--
-- * @mkWDCoreClientM@
-- * @mkWDCoreHoistedClient@
-- * @mkWDCoreComposeTClient@
-- * @mkWDCoreExceptTClient@
--
-- Then you can create a new session from the '_core':
--
-- @
-- ... = do
--   let myWD = mkWDCoreClientM
--   sess <- newSession (_core myWD) mySessionCapabilities
--   ...
-- @
--
-- Then you can access the 'SessionAPI' by passing in the returned 'SessionId', and the
-- commands in the 'SessionAPI' that is returned will be bound to that specific
-- 'SessionId' and cannot be changed. This also saves you from having to include the
-- 'SessionId' as input to every single command.
--
-- @
-- ... = do
--   sess <- ...
--   let sessAPI = _mkSession myWD (_sessionId $ getSuccessValue sess)
--   _ <- navigateTo sessAPI [uri|https://your-page.internet|]
-- @
--
-- The 'ElementAPI' and 'WindowAPI' are bound to the 'SessionAPI' that they are accessed
-- from. Instead of passing in the 'SessionId', you pass in the entire 'SessionAPI'. If
-- you are using the 'WDCore' to interact with the webdriver it is impossible to access an
-- 'ElementId' or send a 'WindowAPI' command in the wrong session.
--
-- In the case of the 'ElementAPI' every command in the record that is returned is bound
-- to that element in that session, you don't have to keep passing the IDs for every
-- command. The set of commands for each element is a separate value.
--
-- @
-- ... = do
--   let sessAPI = ...
--   nameInputId \<- getSuccessValue \<$> findElement sessAPI (ByCss $ input # byId \"name-field\")
--   ageInputId \<- getSuccessValue \<$> findElement sessAPI (ByCss $ input # byId \"age-field\")
--   let
--     -- Commands from this value will *only* interact with the element identified by @nameInputId@
--     nameInputApi = _mkElement myWD sessApi nameInputId
--
--     -- Commands from this value will *only* interact with the element identified by @ageInputId@
--     ageInputApi = _mkElement myWD sessApi ageInputId
-- @
--
data WDCore m = WDCore
  { -- | The root of the API, has functions for checking remote end status and starting new sessions.
    _core      :: WebDriverAPI (AsClientT m)
    -- | General functions for finding elements, sending prompts, scripts, and closing sessions.
  , _mkSession :: SessionId -> SessionAPI (AsClientT m)
    -- | Window related functions, minimising, maximising, creating new windows and finding window handles.
  , _mkWindow  :: SessionAPI (AsClientT m) -> WindowAPI (AsClientT m)
    -- | Specific element related functionality, such as sending keys, retrieving values,
    -- and accessing element properties.
  , _mkElement :: SessionAPI (AsClientT m) -> ElementId -> ElementAPI (AsClientT m)
  }
-- | 'HasWDCore' constraint indicates that a structure contains a 'WDCore'
makeClassy ''WDCore

-- | Build a 'WDCore' using the base 'ClientM' from @servant-client@.
mkWDCoreClientM :: WDCore ClientM
mkWDCoreClientM =
  let
    wdcore = wdClient
  in
    WDCore wdcore
    (fromServant . withSession wdcore)
    (fromServant . withWindow)
    (\sess -> fromServant . withElement sess)

-- | Build a 'WDCore' that is hoisted into your own @m@ using the given transformation.
mkWDCoreHoistedClient :: (forall a. ClientM a -> m a) -> WDCore m
mkWDCoreHoistedClient h =
  let
    wdCoreH = genericClientHoist h
  in
    WDCore wdCoreH
    (fromServant . withSession wdCoreH)
    (fromServant . withWindow)
    (\sess -> fromServant . withElement sess)

-- | Lift the 'ClientM' to 'Compose IO (Either C.ServantError)'.
mkWDCoreComposeTClient :: C.ClientEnv -> WDCore (Compose IO (Either C.ServantError))
mkWDCoreComposeTClient env = mkWDCoreHoistedClient (Compose . flip C.runClientM env)

-- | Lifts the 'ClientM' to 'ExceptT IO C.ServantError'
mkWDCoreExceptTClient :: C.ClientEnv -> WDCore (ExceptT C.ServantError IO)
mkWDCoreExceptTClient env = mkWDCoreHoistedClient (ExceptT . flip C.runClientM env)
