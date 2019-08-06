{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Data.Proxy                                  (Proxy (..))

import           Test.Tasty                                  (TestTree,
                                                              defaultIngredients,
                                                              defaultMainWithIngredients,
                                                              includingOptions,
                                                              testGroup,
                                                              withResource)
import           Test.Tasty.Hedgehog                         (testProperty)
import           Test.Tasty.Options                          (OptionDescription (..))

import           Hedgehog                                    (evalIO,
                                                              executeSequential,
                                                              forAll, property,
                                                              withTests)

import qualified Hedgehog.Gen                                as Gen
import qualified Hedgehog.Range                              as Range

import           Protocol.Webdriver.ClientAPI                (WDCore (..))

import qualified Protocol.Webdriver.ClientAPI                as W
import qualified Protocol.Webdriver.ClientAPI.Types.Internal as W
import qualified Protocol.Webdriver.ClientAPI.Types.Session  as W

import           Commands
import           General.ManageDriver
import           General.TestOpts                            (OverrideWDUrl)
import           General.Types
import           General.UnitTests

managedSession  :: (IO (Env, Sess) -> TestTree) -> IO Env -> TestTree
managedSession f ioenv = withResource initSession endSession f
  where
    endSession (_, Sess _ sCli) =
      W.getSuccessValue <$> W.deleteSession sCli

    initSession = do
      env <- ioenv
      s <- W.getSuccessValue <$> W.newSession (_core . _envWDCore $ env) firefoxSession
      pure (env, Sess (W._sessionId s) (_mkSession (_envWDCore env) (W._sessionId s)))

stateMachineTests :: IO Env -> (Env -> IO ()) -> TestTree
stateMachineTests start stop = withResource start stop (managedSession smt)
  where
    smt ctx = testProperty "Enter some text" . withTests 20 . property $ do
      (env, sessApi) <- evalIO ctx
      let
        commands = fmap (\c -> c env sessApi)
          [ cFindElement
          , cNavigateTo
          , cSendKeys
          , cCheckSentKeys
          , cClearKeys
          ]

      actions <- forAll $ Gen.sequential (Range.linear 5 100) initialModel commands
      evalIO $ W.refresh (_sessClient sessApi)
      executeSequential initialModel actions

main :: IO ()
main = defaultMainWithIngredients myOptions . manageDriverAndServer $ \up down -> testGroup "Webdriver Tests"
  [ testGroup "State Machine" [stateMachineTests up down]
  , unitTests up down
  ]
  where
    myOptions =
      includingOptions [Option (Proxy :: Proxy OverrideWDUrl)]
      : defaultIngredients
