{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module General.Types
  ( Model (..)
  , Env (..)
  , WDCore (..)
  , HasModel (..)
  , WDRun (..)
  , Sess (..)
  , HasSess (..)
  , Threads (..)
  , chromeSession
  ) where

import           Control.Concurrent                         (ThreadId)
import           Control.Monad.IO.Class                     (MonadIO)
import           System.Process                             (ProcessHandle)

import           Control.Lens.TH                            (makeClassy)
import           Data.Kind                                  (Type)

import           Data.Text                                  (Text)

import           Servant.Client                             (ClientM, ClientEnv,
                                                             ServantError)
import           Servant.Client.Generic                     (AsClientT)

import           Hedgehog                                   (MonadTest, Opaque,
                                                             Var)

import           Protocol.Webdriver.ClientAPI               (WebDriverAPI, WindowAPI, ElementAPI,
                                                             SessionAPI)
import           Protocol.Webdriver.ClientAPI.Types.Session (SessionId, NewSession (..))
import           Protocol.Webdriver.ClientAPI.Types.ElementId (ElementId)

import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities as W

data WDCore m = WDCore
  { _core      :: WebDriverAPI (AsClientT m)
  , _mkSession :: SessionId -> SessionAPI (AsClientT m)
  , _mkWindow  :: SessionAPI (AsClientT m) -> WindowAPI (AsClientT m)
  , _mkElement :: SessionAPI (AsClientT m) -> ElementId -> ElementAPI (AsClientT m)
  }
makeClassy ''WDCore

data Sess = Sess
  { _sessId     :: SessionId
  , _sessClient :: SessionAPI (AsClientT IO)
  }
makeClassy ''Sess

chromeSession :: NewSession
chromeSession = NewSession
  (W.asHeadless W.chrome)       -- Browser
  Nothing                       -- username
  Nothing                       -- password

data Model (v :: Type -> Type) = Model
  { _modelAtUrl      :: Bool
  , _modelElementApi :: Maybe (Var (Opaque (ElementAPI (AsClientT IO))) v)
  , _modelKeysSent   :: Maybe Text
  }
makeClassy ''Model

data WDRun m = WDRun
  { runOrFail :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m a
  , runWD :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m (Either ServantError a)
  }

data Threads = Threads
  { webServer :: ThreadId
  , driver    :: Maybe ProcessHandle
  }

data Env = Env
  { _envTestWebServer :: ThreadId
  , _envDriverProcess :: Maybe ProcessHandle
  , _envEnv           :: ClientEnv
  , _envWDCore        :: WDCore IO
  }
