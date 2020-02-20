{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Lens                       (use, (^.), (?=), (.=), (%=))
import           Control.Monad                      (void, unless, when)
import           Control.Monad.IO.Class             (MonadIO)
import Control.Monad.State (MonadState, evalStateT)

import Data.Foldable (forM_)
import           Data.Maybe                         (isNothing)

import           Data.Text                          (Text)

import           Text.URI                           (URI)
import           Text.URI.QQ                        (uri)

import           Hedgehog                           (MonadGen, MonadTest, evalIO, (===), label)
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Internal.Gen              as IGen
import qualified Hedgehog.Range                     as Range

import           Protocol.Webdriver.ClientAPI       (WDCore (..))
import qualified Protocol.Webdriver.ClientAPI       as W
import qualified Protocol.Webdriver.ClientAPI.Types as W

import           Clay.Elements                      (input)
import           Clay.Selector                      (byId, ( # ))

import           General.Types

data Command
  = NavigateTo URI
  | CheckSentKeys
  | SendKeys Text
  | ClearKeys
  | FindElement Text
  deriving (Eq, Show)

nonReservedInput :: MonadGen g => g Char
nonReservedInput =
  -- The control characters U+0000–U+001F and U+007F come from
  -- ASCII. Additionally, U+0080–U+009F were used in conjunction with
  -- ISO 8859 character sets (among others). They are specified in ISO
  -- 6429 and often referred to as C0 and C1 control codes respectively.
  IGen.filterT (\c -> c `notElem` (
    ['\x0' .. '\x001f'] <>
    ['\x007f'] <>
    ['\x0080' .. '\x009f'] <>
    ['\55296'..'\57343'] <>
    W.reservedRange
  )) Gen.unicodeAll

cSendKeys
  :: ( MonadTest m
     , MonadIO m
     , MonadState Model m
     )
  => Text
  -> m ()
cSendKeys inputText = do
  -- Require
  mTargetEl <- use modelElementApi
  checkedKeys <- use modelKeysChecked

  case mTargetEl of
    Just targetEl -> do
      -- Execute
      _ <- evalIO $ W.elementSendKeys targetEl (W.ElementSendKeys inputText)
      -- Update
      modelKeysSent %= Just . maybe inputText (<> inputText)
      label "Send Keys"
    _ -> pure ()

cClearKeys
  :: ( MonadTest m
     , MonadIO m
     , MonadState Model m
     )
  => m ()
cClearKeys = do
  -- Require
  mTargetEl <- use modelElementApi
  checkedKeys <- use modelKeysChecked

  case mTargetEl of
    Just targetEl | checkedKeys -> do
      -- Execute
      _ <- evalIO $ W.getSuccessValue <$> W.elementClear targetEl
      -- Update
      modelKeysSent .= Nothing
      modelKeysChecked .= False
      label "Clear Keys"
    _ -> pure ()

cCheckSentKeys
  :: ( MonadTest m
     , MonadIO m
     , MonadState Model m
     )
  => m ()
cCheckSentKeys = do
  -- Require
  mTargetElem <- use modelElementApi
  mInputText <- use modelKeysSent
  keysChecked <- use modelKeysChecked

  case (mTargetElem, mInputText) of
    (Just targetEl, Just inputText) | not keysChecked -> do
      -- Execute
      val <- evalIO $ W.getSuccessValue <$> W.getElementProperty targetEl "value"
      -- Ensure
      W.Textual inputText === val
      -- Update
      modelKeysChecked .= True
      label "Check sent keys"
    _ -> pure ()

cFindElement
  :: ( MonadTest m
     , MonadIO m
     , MonadState Model m
     )
  => Env
  -> Sess
  -> Text
  -> m ()
cFindElement env (Sess _ sCli) inpId = do
  atUrl <- use modelAtUrl
  hasElem <- use modelElementApi

  -- Require
  when (atUrl && isNothing hasElem) $ do
    -- Execute
    targetElem <- evalIO $ do
      e <- W.getSuccessValue <$> W.findElement sCli (W.ByCss (input # byId inpId))
      pure $ _mkElement (_envWDCore env) sCli e
    -- Update
    modelElementApi .= Just targetElem
    label "Find element"

cNavigateTo
  :: ( MonadTest m
     , MonadIO m
     , MonadState Model m
     )
  => Sess
  -> URI
  -> m ()
cNavigateTo sessApi pageUrl = do
  atUrl <- use modelAtUrl
  -- Require
  unless atUrl $ do
    -- Execute
    _ <- evalIO $ void $ W.navigateTo (sessApi ^. sessClient) (W.WDUri pageUrl)
    -- Update
    modelAtUrl .= True
    label "Navigate to"

evalCommands
  :: ( MonadTest m
     , MonadIO m
     )
  => [Command]
  -> Env
  -> Sess
  -> m ()
evalCommands cmds env currentSession =
  flip evalStateT initialModel $ forM_ cmds $ \case
    NavigateTo u -> cNavigateTo currentSession u
    CheckSentKeys -> cCheckSentKeys
    SendKeys inp -> cSendKeys inp
    ClearKeys -> cClearKeys
    FindElement el -> cFindElement env currentSession el

genCommand :: MonadGen m => m Command
genCommand = Gen.choice
  [ pure ClearKeys
  , pure $ NavigateTo testPage
  , pure CheckSentKeys
  , SendKeys <$> Gen.text (Range.linear 0 100) nonReservedInput
  , FindElement <$> Gen.element ["input-name", "input-occupation"]
  ]
  where
    testPage = [uri|http://localhost:9999/|]
