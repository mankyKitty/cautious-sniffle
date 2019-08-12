{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main (main) where

import           Data.Proxy                   (Proxy (..))

import           Test.Tasty                   (TestTree, defaultIngredients,
                                               defaultMainWithIngredients,
                                               includingOptions, testGroup,
                                               withResource)
import           Test.Tasty.Hedgehog          (testProperty)
import           Test.Tasty.Options           (OptionDescription (..))

import           Hedgehog                     (evalIO, executeSequential,
                                               forAll, property, withTests)

import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range


import qualified Protocol.Webdriver.ClientAPI as W

import           Commands
import           General.ManageDriver
import           General.TestOpts             (OverrideWDUrl)
import           General.Types
import           General.UnitTests            (closeSession, openSession,
                                               testExampleCode, unitTests)

managedSession  :: (IO (Env, Sess) -> TestTree) -> IO Env -> TestTree
managedSession f ioenv = withResource
  (ioenv >>= \env -> (env,) <$> openSession (_envWDCore env))
  (closeSession . snd)
  f

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
  , testExampleCode up down
  ]
  where
    myOptions =
      includingOptions [Option (Proxy :: Proxy OverrideWDUrl)]
      : defaultIngredients
