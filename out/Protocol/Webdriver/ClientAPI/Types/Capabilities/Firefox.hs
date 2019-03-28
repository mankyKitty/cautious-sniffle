{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox where

import           Control.Lens                                   (At (..), Index,
                                                                 IxValue,
                                                                 Ixed (..),
                                                                 makeWrapped,
                                                                 (?~), _Wrapped)
import           Data.Functor.Alt                               ((<!>))
import           Data.Functor.Contravariant                     ((>$<))

import           Data.Text                                      (Text)

import           Data.Dependent.Map                             (DMap)
import           Data.Functor.Identity                          (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Map                                       (Map)
import qualified Data.Map                                       as Map

import           Protocol.Webdriver.ClientAPI.Types.LogSettings (LogSettings,
                                                                 decLogSettings,
                                                                 encLogSettings)

import           Protocol.Webdriver.ClientAPI.Types.Internal    (Base64,
                                                                 decBase64,
                                                                 decodeDMap,
                                                                 dmatKey,
                                                                 encBase64,
                                                                 encodeDMap)

import qualified Waargonaut.Decode                              as D
import qualified Waargonaut.Encode                              as E

-- Capabilities from https://firefox-source-docs.mozilla.org/testing/geckodriver/Capabilities.html

data PrefVal
  = BoolPref Bool
  | TextPref Text
  | NumPref  Int
  deriving (Show, Eq)

decPrefVal :: Monad f => D.Decoder f PrefVal
decPrefVal =
  (BoolPref <$> D.bool) <!>
  (TextPref <$> D.text) <!>
  (NumPref  <$> D.int)

encPrefVal :: Applicative f => E.Encoder f PrefVal
encPrefVal = E.encodeA $ \case
  BoolPref b -> E.runEncoder E.bool b
  TextPref t -> E.runEncoder E.text t
  NumPref  i -> E.runEncoder E.int  i

newtype GeneralFFPrefs = GeneralFFPrefs
  { unGeneralPrefs :: Map Text PrefVal }
  deriving (Show, Eq)
makeWrapped ''GeneralFFPrefs

type instance Index GeneralFFPrefs      = Text
type instance IxValue GeneralFFPrefs    = PrefVal
instance Ixed GeneralFFPrefs where ix i = _Wrapped . ix i
instance At GeneralFFPrefs where at k   = _Wrapped . at k

newPrefs :: Text -> PrefVal -> GeneralFFPrefs
newPrefs k = GeneralFFPrefs . Map.singleton k

setBoolPref :: Text -> Bool -> GeneralFFPrefs -> GeneralFFPrefs
setBoolPref k v = at k ?~ BoolPref v

setTextPref :: Text -> Text -> GeneralFFPrefs -> GeneralFFPrefs
setTextPref k v = at k ?~ TextPref v

setNumPref :: Text -> Int -> GeneralFFPrefs -> GeneralFFPrefs
setNumPref k v = at k ?~ NumPref v

decGeneralPrefs :: Monad f => D.Decoder f GeneralFFPrefs
decGeneralPrefs = GeneralFFPrefs . Map.fromList <$> D.objectAsKeyValues D.text decPrefVal

encGeneralPrefs :: Applicative f => E.Encoder f GeneralFFPrefs
encGeneralPrefs = unGeneralPrefs >$< E.mapToObj encPrefVal id

data FirefoxCap a where
  FFBinary  :: FirefoxCap FilePath
  FFArgs    :: FirefoxCap [Text]
  FFProfile :: FirefoxCap Base64
  FFLog     :: FirefoxCap LogSettings
  FFPrefs   :: FirefoxCap GeneralFFPrefs

deriveGShow ''FirefoxCap
deriveGEq ''FirefoxCap
deriveGCompare ''FirefoxCap
deriveEqTagIdentity ''FirefoxCap
deriveShowTagIdentity ''FirefoxCap

type FirefoxCaps = DMap FirefoxCap Identity

ffCapKeys :: FirefoxCap a -> Text
ffCapKeys FFBinary  = "binary"
ffCapKeys FFArgs    = "args"
ffCapKeys FFProfile = "profile"
ffCapKeys FFLog     = "log"
ffCapKeys FFPrefs   = "prefs"

ffCapEnc :: Applicative f => FirefoxCap a -> E.Encoder f a
ffCapEnc FFBinary  = E.string
ffCapEnc FFArgs    = E.list E.text
ffCapEnc FFProfile = encBase64
ffCapEnc FFLog     = encLogSettings
ffCapEnc FFPrefs   = encGeneralPrefs

decFirefoxCaps :: Monad f => D.Decoder f FirefoxCaps
decFirefoxCaps = decodeDMap
  [ atDM FFBinary D.string
  , atDM FFArgs (D.list D.text)
  , atDM FFProfile decBase64
  , atDM FFLog decLogSettings
  , atDM FFPrefs decGeneralPrefs
  ]
  where
    atDM = dmatKey ffCapKeys

encFirefoxCaps :: Applicative f => E.Encoder f FirefoxCaps
encFirefoxCaps = encodeDMap $ \k -> (ffCapKeys k, ffCapEnc k)
