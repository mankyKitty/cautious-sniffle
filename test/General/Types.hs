{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module General.Types
  ( Model (..)
  , initialModel
  , Env (..)
  , HasModel (..)
  , WDRun (..)
  , Sess (..)
  , HasSess (..)
  , Threads (..)
  , chromeSession
  , firefoxSession
  ) where

import           Control.Concurrent                                      (ThreadId)
import           Control.Monad.IO.Class                                  (MonadIO)
import           System.Process                                          (ProcessHandle)

import           Control.Lens.TH                                         (makeClassy)
import           Data.Function                                           ((&))
import           Data.Text                                               (Text)

import           Servant.Client                                          (ClientEnv,
                                                                          ClientM,
                                                                          ClientError)
import           Servant.Client.Generic                                  (AsClientT)

import           Hedgehog                                                (MonadTest)

import           Protocol.Webdriver.ClientAPI                            (ElementAPI,
                                                                          SessionAPI,
                                                                          WDCore)
import           Protocol.Webdriver.ClientAPI.Types.Session              (NewSession (..),
                                                                          SessionId)

import           Protocol.Webdriver.ClientAPI.Types                      ((~=>))
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities         as W
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox as FF

data Sess = Sess
  { _sessId     :: SessionId
  , _sessClient :: SessionAPI (AsClientT IO)
  }
makeClassy ''Sess

firefoxSession :: NewSession
firefoxSession = NewSession firefox Nothing Nothing
  where
    firefox = W.firefox
      & W.PlatformName ~=> W.Linux
      & W.FirefoxSettings ~=> ffSettings

    ffSettings = mempty
        -- I needed to add this as Mozilla removed this setting, but my
        -- geckdriver keeps trying to set it to an invalid value and Marionette crashes. :<
      & FF.FFPrefs ~=> FF.newPrefs "app.update.auto" (FF.TextPref "no")
      & FF.FFArgs ~=> ["--headless"]

chromeSession :: NewSession
chromeSession = NewSession
  (W.asHeadless W.chrome)       -- Browser
  Nothing                       -- username
  Nothing                       -- password

data Model = Model
  { _modelAtUrl       :: Bool
  , _modelElementApi  :: Maybe (ElementAPI (AsClientT IO))
  , _modelKeysSent    :: Maybe Text
  , _modelKeysChecked :: Bool
  }
makeClassy ''Model

initialModel :: Model
initialModel = Model False Nothing Nothing False

data WDRun m = WDRun
  { runOrFail :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m a
  , runWD :: forall a. (MonadTest m, MonadIO m) => ClientM a -> m (Either ClientError a)
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
