{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands where

import           Control.Lens                                        ((.~),
                                                                      (?~),
                                                                      (^.))
import           Control.Monad.IO.Class                              (MonadIO,liftIO)

import           Data.Function                                       ((&))
import           Data.Kind                                           (Type)
import           Data.Maybe                                          (isJust, isNothing)
import           Data.Text                                           (Text)
import qualified Data.Text                                           as T
import           Data.Typeable                                       (Typeable)

import           Text.URI                                            (URI)
import           Text.URI.QQ                                         (uri)

import Servant.API (NoContent)
import qualified Servant.Client                                      as SC

import           Hedgehog                                            (Callback (..),
                                                                      Var (..),
                                                                      Command (..),
                                                                      Concrete,
                                                                      HTraversable (..),
                                                                      MonadGen,
                                                                      MonadTest,
                                                                      Symbolic,
                                                                      assert,
                                                                      evalIO,
                                                                      failure,
                                                                      (===), concrete)
import qualified Hedgehog.Gen                                        as Gen
import qualified Hedgehog.Range                                        as Range

import qualified Protocol.Webdriver.ClientAPI                        as W
import qualified Protocol.Webdriver.ClientAPI.Types                  as W
import qualified Protocol.Webdriver.ClientAPI.Types.Capabilities     as W
import qualified Protocol.Webdriver.ClientAPI.Types.ElementId        as W
import           Protocol.Webdriver.ClientAPI.Types.Internal         as W
import qualified Protocol.Webdriver.ClientAPI.Types.LocationStrategy as W
import           Protocol.Webdriver.ClientAPI.Types.Session          (SessionId)
import qualified Protocol.Webdriver.ClientAPI.Types.Session          as W

import           Clay.Elements                                       (input)
import           Clay.Selector                                       (byId,
                                                                      ( # ))
import           Types

type WDCmd = forall g m.
  ( MonadGen g
  , MonadTest m
  , MonadIO m
  )
  => WDRun m
  -> SessionId
  -> Command g m Model

newtype Cmd a (v :: Type -> Type) = Cmd a
  deriving (Eq, Show)

instance HTraversable (Cmd a) where htraverse _ (Cmd a) = pure (Cmd a)

newtype LoadUrl = LoadUrl URI deriving (Eq, Show)
data GetTextInput = GetTextInput Text deriving Show

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
    gen m = case _modelTextInputElement m of
      Nothing -> Nothing
      Just varElem -> pure $ SendKeys varElem <$> Gen.text (Range.linear 0 100) Gen.unicode

    exec :: SendKeys Concrete -> m Text
    exec (SendKeys elemId textInput) = runOrFail run $ do
      _ <- W.elementSendKeys sessId (concrete elemId) (W.ElementSendKeys textInput)
      W.getElementAttribute sessId (concrete elemId) "value"
  in
    Command gen exec
    [ Require $ \m _ -> isJust $ _modelTextInputElement m
    , Ensure $ \_ _ (SendKeys _ inp) val -> inp === val
    ]


cFindElement :: WDCmd
cFindElement run sessId =
  let
    readyFindElem m = _modelAtUrl m
      && isNothing (_modelTextInputElement m)

    gen m =
      if readyFindElem m then
        Just $ Cmd . GetTextInput <$> Gen.element ["input-name", "input-element"]
      else
        Nothing

    exec (Cmd (GetTextInput inpId)) = runOrFail run $ do
      W.unValue <$> W.findElement sessId (W.ByCss (input # byId inpId))
  in
    Command gen exec
    [ Require $ \m _ -> readyFindElem m
    , Update $ \m _ out -> m & modelTextInputElement .~ Just out
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
    gen m =
      if _modelAtUrl m then
        Nothing
      else
        pure $ Cmd <$> Gen.constant (LoadUrl [uri|http://localhost:9999/|])

    exec (Cmd (LoadUrl page)) = runOrFail run $ do
      _ <- W.navigateTo sessId (W.WDUri page)
      W.unValue <$> W.getUrl sessId
  in
    Command gen exec
      [ Require $ \m _ -> not $ _modelAtUrl m
      , Update $ \m _ _ -> m & modelAtUrl .~ True
      , Ensure $ \_old _new (Cmd (LoadUrl url0)) url1 -> W._unWDUri url1 === url0
      ]
