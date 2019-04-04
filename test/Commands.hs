{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Lens                                        ((.~),
                                                                      (?~))
import           Control.Monad.IO.Class                              (MonadIO)

import           Data.Function                                       ((&))
import           Data.Kind                                           (Type)
import           Data.Maybe                                          (isJust,
                                                                      isNothing)
import           Data.Text                                           (Text)
import qualified Data.Text                                           as T

import           Text.URI                                            (URI)
import           Text.URI.QQ                                         (uri)

import           Hedgehog                                            (Callback (..),
                                                                      Command (..),
                                                                      Concrete,
                                                                      HTraversable (..),
                                                                      MonadGen,
                                                                      MonadTest,
                                                                      Symbolic,
                                                                      Var (..),
                                                                      assert,
                                                                      concrete,
                                                                      (===))
import qualified Hedgehog.Gen                                        as Gen
import qualified Hedgehog.Range                                      as Range

import qualified Protocol.Webdriver.ClientAPI                        as W
import qualified Protocol.Webdriver.ClientAPI.Types                  as W
-- import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities     as W
import qualified Protocol.Webdriver.ClientAPI.Types.ElementId        as W
import           Protocol.Webdriver.ClientAPI.Types.Internal         as W
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy as W
import           Protocol.Webdriver.ClientAPI.Types.Session          (SessionId)
-- import qualified Protocol.Webdriver.ClientAPI.Types.Session          as W

import           Clay.Elements                                       (input)
import           Clay.Selector                                       (byId,
                                                                      ( # ))
import           Types

newtype Cmd a (v :: Type -> Type) = Cmd a
  deriving (Eq, Show)

instance HTraversable (Cmd a) where htraverse _ (Cmd a) = pure (Cmd a)

newtype LoadUrl = LoadUrl URI deriving (Eq, Show)
newtype GetTextInput = GetTextInput Text deriving Show

data SendKeys (v :: Type -> Type) = SendKeys (Var W.ElementId v) Text
  deriving Show

instance HTraversable SendKeys where
  htraverse f (SendKeys velem inp) =
    (`SendKeys` inp) <$> htraverse f velem

cSendKeys
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> SessionId
  -> Command g m Model
cSendKeys run sessId =
  let
    gen :: Model Symbolic -> Maybe (g (SendKeys Symbolic))
    gen m = case _modelTextInputElement m of
      Nothing ->
        Nothing
      Just varElem ->
        pure $ SendKeys varElem <$> Gen.text (Range.linear 0 100) Gen.unicode

    exec :: SendKeys Concrete -> m Text
    exec (SendKeys elemId textInput) = runOrFail run $ do
      _ <- W.elementSendKeys sessId (concrete elemId) (W.ElementSendKeys textInput)
      W.unValue <$> W.getElementProperty sessId (concrete elemId) "value"

  in
    Command gen exec
    [ Require $ \m _                     -> isJust $ _modelTextInputElement m
    , Ensure $ \_ _ (SendKeys _ inp) val -> inp === val
    ]

cFindElement
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> SessionId
  -> Command g m Model
cFindElement run sessId =
  let
    readyFindElem m = _modelAtUrl m
      && isNothing (_modelTextInputElement m)

    gen :: Model Symbolic -> Maybe (g (Cmd GetTextInput Symbolic))
    gen m = if readyFindElem m then
              Just $ Cmd . GetTextInput <$> Gen.element ["input-name", "input-occupation"]
            else
              Nothing

    exec :: Cmd GetTextInput Concrete -> m W.ElementId
    exec (Cmd (GetTextInput inpId)) = runOrFail run $
      W.unValue <$> W.findElement sessId (W.ByCss (input # byId inpId))
  in
    Command gen exec
    [ Require $ \m _                    -> readyFindElem m
    , Update $ \m _ out                 -> m & modelTextInputElement ?~ out
    , Ensure $ \_ _ _ (W.ElementId eid) -> assert . not . T.null $ eid
    ]

cNavigateTo
  :: forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> SessionId
  -> Command g m Model
cNavigateTo run sessId =
  let
    gen :: Model Symbolic -> Maybe (g (Cmd LoadUrl Symbolic))
    gen m = if _modelAtUrl m then
              Nothing
            else
              pure $ Cmd <$> Gen.constant (LoadUrl [uri|http://localhost:9999/|])

    exec :: Cmd LoadUrl Concrete -> m W.WDUri
    exec (Cmd (LoadUrl page)) = runOrFail run $ do
      _ <- W.navigateTo sessId (W.WDUri page)
      W.unValue <$> W.getUrl sessId
  in
    Command gen exec
      [ Require $ \m _                                -> not $ _modelAtUrl m
      , Update $ \m _ _                               -> m & modelAtUrl .~ True
      , Ensure $ \_old _new (Cmd (LoadUrl url0)) url1 -> W._unWDUri url1 === url0
      ]
