{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module General.Types
  ( Model (..)
  , Env (..)
  , HasModel (..)
  , WDRun (..)
  , Sess (..)
  , HasSess (..)
  , Threads (..)
  , chromeSession
  ) where

import           Control.Concurrent                              (ThreadId)
import           Control.Monad.IO.Class                          (MonadIO)
import           System.Process                                  (ProcessHandle)

import           Control.Lens.TH                                 (makeClassy)
import           Data.Kind                                       (Type)

import           Data.Text                                       (Text)

import           Servant.Client                                  (ClientEnv,
                                                                  ClientM,
                                                                  ServantError)
import           Servant.Client.Generic                          (AsClientT)

import           Hedgehog                                        (MonadTest,
                                                                  Opaque, Var)

import           Protocol.Webdriver.ClientAPI                    (ElementAPI,
                                                                  SessionAPI,
                                                                  WDCore)
import           Protocol.Webdriver.ClientAPI.Types.Session      (NewSession (..),
                                                                  SessionId)

import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities as W

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
