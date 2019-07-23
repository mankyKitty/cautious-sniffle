{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module General.TestAPI where

import           Test.Tasty.HUnit             (assertFailure)

import           Control.Monad                ((>=>))
import           Control.Monad.Catch          (MonadThrow, displayException,
                                               throwM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)

import           Servant.API.Generic
import qualified Servant.Client               as C
import           Servant.Client.Generic       (genericClientHoist)

import qualified Protocol.Webdriver.ClientAPI as W

import           General.Types                (WDCore (..))

liftThrow :: (MonadThrow m, MonadIO m) => C.ClientEnv -> C.ClientM a -> m a
liftThrow env = liftIO . flip C.runClientM env >=> either throwM pure

liftTest :: C.ClientEnv -> C.ClientM a -> IO a
liftTest env = flip C.runClientM env >=> either (assertFailure . displayException) pure

mkWDCore :: (forall a. C.ClientEnv -> C.ClientM a -> m a) -> C.ClientEnv -> WDCore m
mkWDCore nt env =
  let
    wdcore = genericClientHoist (nt env)
  in
    WDCore wdcore
    (fromServant . W.withSession wdcore)
    (fromServant . W.withWindow)
    (\sess -> fromServant . W.withElement sess)

mkWDCoreTest :: C.ClientEnv -> WDCore IO
mkWDCoreTest = mkWDCore liftTest

mkWDCoreThrow :: (MonadThrow m, MonadIO m) => C.ClientEnv -> WDCore m
mkWDCoreThrow = mkWDCore liftThrow
