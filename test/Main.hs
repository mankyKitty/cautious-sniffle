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

import           Hedgehog                     (evalIO,
                                               forAll, property, withTests)

import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

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

prop_textInput :: IO Env -> (Env -> IO ()) -> TestTree
prop_textInput start stop = withResource start stop (managedSession smt)
  where
    smt ctx = testProperty "Enter some text." . withTests 20 . property $ do
      (env, sessApi) <- evalIO ctx
      cmds <- forAll $ Gen.list (Range.linear 0 20) genCommand
      evalCommands cmds env sessApi

main :: IO ()
main = defaultMainWithIngredients myOptions . manageDriverAndServer $ \up down -> testGroup "Webdriver Tests"
  [ testGroup "State Machine" [prop_textInput up down]
  , unitTests up down
  , testExampleCode up down
  ]
  where
    myOptions =
      includingOptions [Option (Proxy :: Proxy OverrideWDUrl)]
      : defaultIngredients
