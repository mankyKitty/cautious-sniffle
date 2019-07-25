{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Monad                               (void)
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

stateMachineTests :: IO Env -> (Env -> IO ()) -> TestTree
stateMachineTests start stop = withResource start stop $ \ioenv ->
  testProperty "Enter some text" . withTests 1 . property $ do
    env <- evalIO ioenv
    sessD <- evalIO $ W.getSuccessValue <$> W.newSession (_core . _envWDCore $ env) chromeSession

    let
      sCli = _mkSession (_envWDCore env) (W._sessionId sessD)
      sessApi = Sess (W._sessionId sessD) sCli

      initialModel = Model
        False
        Nothing
        Nothing

      commands = fmap (\c -> c env sessApi)
        [ cFindElement
        , cNavigateTo
        , cSendKeys
        , cCheckSentKeys
        ]

    actions <- forAll $ Gen.sequential (Range.linear 3 10) initialModel commands
    executeSequential initialModel actions
    evalIO . void $ W.deleteSession sCli

main :: IO ()
main = defaultMainWithIngredients myOptions . manageDriverAndServer $ \up down -> testGroup "Webdriver Tests"
  [ stateMachineTests up down
  , unitTests up down
  ]
  where
    myOptions =
      includingOptions [Option (Proxy :: Proxy OverrideWDUrl)]
      : defaultIngredients
