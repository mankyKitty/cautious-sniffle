{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types
  ( Model (..)
  , HasModel (..)
  , WDRun (..)
  ) where

import           Control.Monad.IO.Class                       (MonadIO)

import           Control.Lens.TH                              (makeClassy)
import           Data.Kind                                    (Type)

import           Servant.Client                               (ClientM,
                                                               ServantError)

import           Hedgehog                                     (MonadTest, Var)

import           Protocol.Webdriver.ClientAPI.Types.ElementId (ElementId)

data Model (v :: Type -> Type) = Model
  { _modelAtUrl      :: Bool
  , _modelTextInputElement :: Maybe (Var ElementId v)
  }
  deriving Show
makeClassy ''Model

data WDRun m = WDRun
  { runOrFail :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m a
  , runWD :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m (Either ServantError a)
  }
