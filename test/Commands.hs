{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Applicative                (liftA2)
import           Control.Lens                       (to, (.~), (?~), (^.), (^?),
                                                     _Just)
import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO)

import           Data.Function                      ((&))
import           Data.Kind                          (Type)
import           Data.Maybe                         (isJust, isNothing)

import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as TE

import           Text.URI                           (URI)
import           Text.URI.QQ                        (uri)

import           Servant.Client.Generic             (AsClientT)

import           Hedgehog                           (Callback (..), failure,
                                                     Command (..), Concrete,
                                                     HTraversable (..),
                                                     MonadGen, MonadTest,
                                                     Opaque (..), Symbolic,
                                                     Var (..), evalIO, opaque,
                                                     (===))
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Internal.Gen              as IGen
import qualified Hedgehog.Range                     as Range

import           Protocol.Webdriver.ClientAPI       (WDCore (..))
import qualified Protocol.Webdriver.ClientAPI       as W
import qualified Protocol.Webdriver.ClientAPI.Types as W

import           Clay.Elements                      (input)
import           Clay.Selector                      (byId, ( # ))

import           General.Types

newtype Cmd a (v :: Type -> Type) = Cmd a
  deriving (Eq, Show)

instance HTraversable (Cmd a) where htraverse _ (Cmd a) = pure (Cmd a)

newtype LoadUrl = LoadUrl URI deriving (Eq, Show)
newtype GetTextInput = GetTextInput Text deriving Show

data CheckSentKeys (v :: Type -> Type) = CheckSentKeys (Var (Opaque (W.ElementAPI (AsClientT IO))) v) Text
  deriving Show

instance HTraversable CheckSentKeys where
  htraverse f (CheckSentKeys eApi inp) = (`CheckSentKeys` inp) <$> htraverse f eApi

data SendKeys (v :: Type -> Type) = SendKeys (Var (Opaque (W.ElementAPI (AsClientT IO))) v) Text 
  deriving Show

instance HTraversable SendKeys where
  htraverse f (SendKeys eApi inp) = (`SendKeys` inp) <$> htraverse f eApi

newtype ClearKeys (v :: Type -> Type) = ClearKeys (Var (Opaque (W.ElementAPI (AsClientT IO))) v)
  deriving Show

instance HTraversable ClearKeys where
  htraverse f (ClearKeys eApi) = ClearKeys <$> htraverse f eApi

boolGen :: MonadGen g => (Model Symbolic -> Bool) -> g (a Symbolic) -> Model Symbolic -> Maybe (g (a Symbolic))
boolGen f g m = if f m then pure g else Nothing

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
    W.reservedUnicodeRange
  )) Gen.unicodeAll

cSendKeys :: forall g m. (MonadGen g, MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cSendKeys _ _ =
  let
    gen :: Model Symbolic -> Maybe (g (SendKeys Symbolic))
    gen m | m ^. modelKeysChecked . to not = case (m ^. modelElementApi, m ^. modelKeysSent) of
              (Just eApi, Nothing) -> pure $ SendKeys eApi <$> 
                Gen.text (Range.linear 0 100) nonReservedInput
              _                    -> Nothing
          | otherwise = Nothing

    exec :: SendKeys Concrete -> m ()
    exec (SendKeys elemApi textInput) = evalIO . void $
      W.elementSendKeys (opaque elemApi) (W.ElementSendKeys textInput)

  in
    Command gen exec
    [ Require $ \m _ -> isJust (_modelElementApi m) && isNothing (_modelKeysSent m) && not (_modelKeysChecked m)
    , Update $ \m (SendKeys _ sent) _ -> m & modelKeysSent ?~ sent
    ]

cClearKeys :: forall g m. (MonadGen g , MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cClearKeys _ _ =
  let
    gen :: Model Symbolic -> Maybe (g (ClearKeys Symbolic))
    gen m | m ^. modelKeysChecked = m ^? modelElementApi . _Just . to (pure . ClearKeys)
          | otherwise             = Nothing

    exec :: ClearKeys Concrete -> m ()
    exec (ClearKeys eApi) = evalIO $ W.getSuccessValue <$> W.elementClear (opaque eApi)
  in
    Command gen exec
    [ Require $ \m _ -> m ^. modelKeysChecked && isJust (m ^. modelKeysSent)
    , Update $ \m _ _ -> m
                         & modelKeysSent .~ Nothing
                         & modelKeysChecked .~ False
    ]

cCheckSentKeys :: forall g m. (MonadGen g , MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cCheckSentKeys _ _sess =
  let
    canCheckSentKeys m
      | m ^. modelKeysChecked . to not = liftA2 (,) (m ^. modelElementApi) (m ^. modelKeysSent)
      | otherwise                      = Nothing

    gen :: Model Symbolic -> Maybe (g (CheckSentKeys Symbolic))
    gen m = pure . uncurry CheckSentKeys <$> canCheckSentKeys m

    exec :: CheckSentKeys Concrete -> m W.PropertyVal
    exec (CheckSentKeys eApi _) = evalIO $
      W.getSuccessValue <$> W.getElementProperty (opaque eApi) "value"
  in
    Command gen exec
    [ Require $ \m (CheckSentKeys _ ks) ->
        (isJust $ canCheckSentKeys m) && maybe False (== ks) (m ^. modelKeysSent)
    , Update $ \m _ _ ->
        m & modelKeysChecked .~ True
    , Ensure $ \_ _ (CheckSentKeys _ inp) out -> case out of
        W.Textual u -> TE.encodeUtf8 inp === u
        _           -> failure
    ]

cFindElement :: forall g m. (MonadGen g , MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cFindElement env (Sess _ sCli) =
  let
    readyFindElem m = m ^. modelAtUrl && isNothing (_modelElementApi m)

    gen :: Model Symbolic -> Maybe (g (Cmd GetTextInput Symbolic))
    gen = boolGen readyFindElem (Cmd . GetTextInput <$> Gen.element ["input-name", "input-occupation"])

    exec :: Cmd GetTextInput Concrete -> m (Opaque (W.ElementAPI (AsClientT IO)))
    exec (Cmd (GetTextInput inpId)) = evalIO $ do
      e <- W.getSuccessValue <$> W.findElement sCli (W.ByCss (input # byId inpId))
      pure . Opaque $ _mkElement (_envWDCore env) sCli e

  in
    Command gen exec
    [ Require $ \m _ -> readyFindElem m
    , Update $ \m _ out -> m & modelElementApi ?~ out
    ]

cNavigateTo :: forall g m. (MonadGen g , MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cNavigateTo _ sessApi =
  let
    gen :: Model Symbolic -> Maybe (g (Cmd LoadUrl Symbolic))
    gen m = if m ^. modelAtUrl . to not
      then pure $ Cmd <$> Gen.constant (LoadUrl [uri|http://localhost:9999/|])
      else Nothing

    exec :: Cmd LoadUrl Concrete -> m ()
    exec (Cmd (LoadUrl page)) = evalIO $
      void $ W.navigateTo (sessApi ^. sessClient) (W.WDUri page)
  in
    Command gen exec
      [ Require $ \m _  -> not $ _modelAtUrl m
      , Update $ \m _ _ -> m & modelAtUrl .~ True
      ]
