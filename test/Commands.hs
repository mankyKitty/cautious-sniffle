{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Applicative                (liftA2)
import           Control.Lens                       (to, (.~), (?~), (^.))
import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO)

import           Data.Function                      ((&))
import           Data.Kind                          (Type)
import           Data.Maybe                         (isJust, isNothing)
import           Data.Text                          (Text)

import           Text.URI                           (URI)
import           Text.URI.QQ                        (uri)

import           Servant.Client.Generic             (AsClientT)

import           Hedgehog                           (Callback (..),
                                                     Command (..), Concrete,
                                                     HTraversable (..),
                                                     MonadGen, MonadTest,
                                                     Opaque (..), Symbolic,
                                                     Var (..), evalIO, opaque,
                                                     (===))
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

import Protocol.Webdriver.ClientAPI       (WDCore (..))
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

boolGen :: MonadGen g => (Model Symbolic -> Bool) -> g (a Symbolic) -> Model Symbolic -> Maybe (g (a Symbolic))
boolGen f g m = if f m then pure g else Nothing

cSendKeys :: forall g m. (MonadGen g, MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cSendKeys _ _ =
  let
    gen :: Model Symbolic -> Maybe (g (SendKeys Symbolic))
    gen m = case (m ^. modelElementApi, m ^. modelKeysSent) of
      (Just eApi, Nothing) -> pure $ SendKeys eApi <$> Gen.text (Range.linear 0 100) Gen.unicodeAll
      _                    -> Nothing

    exec :: SendKeys Concrete -> m ()
    exec (SendKeys elemApi textInput) = evalIO . void $
      W.elementSendKeys (opaque elemApi) (W.ElementSendKeys textInput)

  in
    Command gen exec
    [ Require $ \m _ -> isJust (_modelElementApi m) && isNothing (_modelKeysSent m)
    , Update $ \m (SendKeys _ sent) _ -> m & modelKeysSent ?~ sent
    ]

cCheckSentKeys :: forall g m. (MonadGen g , MonadTest m , MonadIO m) => Env -> Sess -> Command g m Model
cCheckSentKeys _ _sess =
  let
    canCheckSentKeys m = liftA2 (,)
      (m ^. modelElementApi)
      (m ^. modelKeysSent)

    gen :: Model Symbolic -> Maybe (g (CheckSentKeys Symbolic))
    gen m = pure . uncurry CheckSentKeys <$> canCheckSentKeys m

    exec :: CheckSentKeys Concrete -> m Text
    exec (CheckSentKeys eApi _) = evalIO $
      W.getSuccessValue <$> W.getElementProperty (opaque eApi) "value"
  in
    Command gen exec
    [ Require $ const . isJust . canCheckSentKeys
    , Update $ \m _ _ -> m & modelKeysSent .~ Nothing
    , Ensure $ \_ _ (CheckSentKeys _ txt) out -> txt === out
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
