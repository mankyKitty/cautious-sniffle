{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types
  ( Model (..)
  , HasModel (..)
  , WDRun (..)
  , Sess (..)
  , HasSess (..)
  ) where

import           Control.Monad.IO.Class                       (MonadIO)

import           Control.Lens.TH                              (makeClassy)
import           Data.Kind                                    (Type)

import           Data.Text                                    (Text)

import           Servant.Client                               (ClientM,
                                                               ServantError)
import           Servant.Client.Generic                       (AsClientT)

import           Hedgehog                                     (MonadTest,
                                                               Opaque, Var)

import           Protocol.Webdriver.ClientAPI                 (ElementAPI,
                                                               SessionAPI)
import           Protocol.Webdriver.ClientAPI.Types.Session   (SessionId)

data Sess = Sess
  { _sessId     :: SessionId
  , _sessClient :: SessionAPI (AsClientT ClientM)
  }
makeClassy ''Sess

data Model (v :: Type -> Type) = Model
  { _modelAtUrl      :: Bool
  , _modelElementApi :: Maybe (Var (Opaque (ElementAPI (AsClientT ClientM))) v)
  , _modelKeysSent   :: Maybe Text
  }
makeClassy ''Model

data WDRun m = WDRun
  { runOrFail :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m a
  , runWD :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m (Either ServantError a)
  }
