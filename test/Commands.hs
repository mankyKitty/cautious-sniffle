{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Lens                                        (to, (.~),
                                                                      (?~),
                                                                      (^.))
import           Control.Monad                                       (void)
import           Control.Monad.IO.Class                              (MonadIO)

import           Data.Function                                       ((&))
import           Data.Kind                                           (Type)
import           Data.Maybe                                          (isJust,
                                                                      isNothing)
import           Data.Text                                           (Text)

import           Text.URI                                            (URI)
import           Text.URI.QQ                                         (uri)

import           Servant.Client                                      (ClientM)
import           Servant.Client.Generic                              (AsClientT)

import           Hedgehog                                            (Callback (..),
                                                                      Command (..),
                                                                      Concrete,
                                                                      HTraversable (..),
                                                                      MonadGen,
                                                                      MonadTest,
                                                                      Opaque (..),
                                                                      Symbolic,
                                                                      Var (..),
                                                                      opaque,
                                                                      (===))
import qualified Hedgehog.Gen                                        as Gen
import qualified Hedgehog.Range                                      as Range

import qualified Protocol.Webdriver.ClientAPI.GENERICS               as W
import qualified Protocol.Webdriver.ClientAPI.Types                  as W
import           Protocol.Webdriver.ClientAPI.Types.Internal         as W
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy as W

import           Clay.Elements                                       (input)
import           Clay.Selector                                       (byId,
                                                                      ( # ))
import           Types

newtype Cmd a (v :: Type -> Type) = Cmd a
  deriving (Eq, Show)

instance HTraversable (Cmd a) where htraverse _ (Cmd a) = pure (Cmd a)

newtype LoadUrl = LoadUrl URI deriving (Eq, Show)
newtype GetTextInput = GetTextInput Text deriving Show

data SendKeys (v :: Type -> Type) = SendKeys (Var (Opaque (W.ElementAPI (AsClientT ClientM))) v) Text
  deriving Show

instance HTraversable SendKeys where
  htraverse f (SendKeys eApi inp) = (`SendKeys` inp) <$> htraverse f eApi

boolGen
  :: MonadGen g
  => (Model Symbolic -> Bool)
  -> g (a Symbolic)
  -> Model Symbolic
  -> Maybe (g (a Symbolic))
boolGen f g m =
  if f m then pure g else Nothing

cSendKeys
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> Sess
  -> Command g m Model
cSendKeys run _sess =
  let
    gen :: Model Symbolic -> Maybe (g (SendKeys Symbolic))
    gen m = case (_modelElementApi m, _modelKeysSent m) of
      (Just eApi, Nothing) -> pure $ SendKeys eApi <$> Gen.text (Range.linear 0 100) Gen.unicode
      _                    -> Nothing

    exec :: SendKeys Concrete -> m ()
    exec (SendKeys elemApi textInput) = runOrFail run .
      void $ W.elementSendKeys (opaque elemApi) (W.ElementSendKeys textInput)

  in
    Command gen exec
    [ Require $ \m _ -> isJust (_modelElementApi m) && isNothing (_modelKeysSent m)
    , Update $ \m (SendKeys _ sent) _ -> m & modelKeysSent ?~ sent
    ]

cFindElement
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> Sess
  -> Command g m Model
cFindElement run sess=
  let
    readyFindElem m = m ^. modelAtUrl && isNothing (_modelElementApi m)

    gen :: Model Symbolic -> Maybe (g (Cmd GetTextInput Symbolic))
    gen = boolGen readyFindElem (Cmd . GetTextInput <$> Gen.element ["input-name", "input-occupation"])

    exec :: Cmd GetTextInput Concrete -> m (Opaque (W.ElementAPI (AsClientT ClientM)))
    exec (Cmd (GetTextInput inpId)) = runOrFail run $
      Opaque . W.elementClient (_sessId sess) . W.unValue <$>
        W.findElement (_sessClient sess) (W.ByCss (input # byId inpId))

  in
    Command gen exec
    [ Require $ \m _ -> readyFindElem m
    , Update $ \m _ out -> m & modelElementApi ?~ out
    ]

cNavigateTo
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> Sess
  -> Command g m Model
cNavigateTo run sess =
  let
    gen :: Model Symbolic -> Maybe (g (Cmd LoadUrl Symbolic))
    gen m = if m ^. modelAtUrl . to not
      then pure $ Cmd <$> Gen.constant (LoadUrl [uri|http://localhost:9999/|])
      else Nothing

    exec :: Cmd LoadUrl Concrete -> m W.WDUri
    exec (Cmd (LoadUrl page)) = runOrFail run $ do
      _ <- W.navigateTo (_sessClient sess) (W.WDUri page)
      W.unValue <$> W.getUrl (_sessClient sess)
  in
    Command gen exec
      [ Require $ \m _                                -> not $ _modelAtUrl m
      , Update $ \m _ _                               -> m & modelAtUrl .~ True
      , Ensure $ \_old _new (Cmd (LoadUrl url0)) url1 -> W._unWDUri url1 === url0
      ]
