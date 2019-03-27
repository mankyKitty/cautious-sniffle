{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Protocol.Webdriver.ClientAPI.Types.Capabilities.Firefox where

import           Control.Lens                                   (makeWrapped)
import           Data.Functor.Contravariant                     ((>$<))

import           Data.Text                                      (Text)

import           Data.Dependent.Map                             (DMap)
import           Data.Functor.Identity                          (Identity (..))
import           Data.GADT.Compare.TH
import           Data.GADT.Show.TH
import           Data.Map                                       (Map)

import           Protocol.Webdriver.ClientAPI.Types.LogSettings (LogSettings, encodeLogSettings)

import           Protocol.Webdriver.ClientAPI.Types.Internal    (Base64,
                                                                 encodeBase64,
                                                                 encodeDMap)

import qualified Waargonaut.Encode                              as E

-- Capabilities from https://firefox-source-docs.mozilla.org/testing/geckodriver/Capabilities.html

data PrefVal
  = BoolPref Bool
  | TextPref Text
  | NumPref  Int
  deriving (Show, Eq)

encodePrefVal :: Applicative f => E.Encoder f PrefVal
encodePrefVal = E.encodeA $
  \case BoolPref b -> E.runEncoder E.bool b
        TextPref t -> E.runEncoder E.text t
        NumPref  i -> E.runEncoder E.int  i

newtype GeneralFFPrefs = GeneralFFPrefs
  { unGeneralPrefs :: Map Text PrefVal }
  deriving (Show, Eq)
makeWrapped ''GeneralFFPrefs

encodeGeneralPrefs :: Applicative f => E.Encoder f GeneralFFPrefs
encodeGeneralPrefs = unGeneralPrefs >$< E.mapToObj encodePrefVal id

data FirefoxCap a where
  FFBinary  :: FirefoxCap FilePath
  FFArgs    :: FirefoxCap [Text]
  FFProfile :: FirefoxCap Base64
  FFLog     :: FirefoxCap LogSettings
  FFPrefs   :: FirefoxCap GeneralFFPrefs

deriveGShow ''FirefoxCap
deriveGEq ''FirefoxCap
deriveGCompare ''FirefoxCap

type FirefoxCaps = DMap FirefoxCap Identity

encodeFirefoxCaps :: Applicative f => E.Encoder f FirefoxCaps
encodeFirefoxCaps = encodeDMap $
  \case FFBinary  -> ("binary", E.string)
        FFArgs    -> ("args", E.list E.text)
        FFProfile -> ("profile", encodeBase64)
        FFLog     -> ("log", encodeLogSettings)
        FFPrefs   -> ("prefs", encodeGeneralPrefs)
