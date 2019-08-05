{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Locating elements on the page is done via various location strategies.
--
-- The most obvious of which is using CSS locators, but a few other options are available
-- as well. To help write the CSS, the [clay](https://hackage.haskell.org/package/clay)
-- package is used.
--
module Protocol.Webdriver.ClientAPI.Types.LocationStrategy where

import           Control.Monad                               ((>=>))
import           Control.Monad.Error.Lens                    (throwing)
import           Data.Text                                   (Text)
import           Data.Text.Lazy                              (toStrict)
import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Decode.Error                     as DE
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..),
                                                              JsonEncode (..))

import           Clay.Render                                 (renderSelector)
import           Clay.Selector                               (Selector,
                                                              selectorFromText)

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)

-- | The choices for how to locate element(s).
data LocateUsing
  = ByCss Selector
  | LinkText Text
  | PartialLinkText Text
  | TagName Text
  | XPath Text
  deriving (Show)

instance Eq LocateUsing where
  (==) (LinkText a)        (LinkText b)         = a == b
  (==) (PartialLinkText a) (PartialLinkText b)  = a == b
  (==) (TagName a)         (TagName b)          = a == b
  (==) (XPath a)           (XPath b)            = a == b
  -- clay doesn't provide an Eq instance for Selector, this seems sufficient?
  (==) (ByCss a)           (ByCss b)            = renderSelector a == renderSelector b
  (==) _                   _                    = False

instance JsonEncode WDJson LocateUsing where
  mkEncoder = pure . E.mapLikeObj $ \lc ->
    let
      (u,v) = case lc of
        ByCss sel           -> ("css selector", toStrict $ renderSelector sel)
        LinkText txt        -> ("link text", txt)
        PartialLinkText txt -> ("partial link text", txt)
        TagName tag         -> ("tag name", tag)
        XPath xp            -> ("xpath", xp)
    in
      E.atKey' "using" E.text u .
      E.atKey' "value" E.text v

instance JsonDecode WDJson LocateUsing where
  mkDecoder = pure . D.withCursor $ D.down >=> \c -> do
    u <- D.fromKey "using" D.text c
    v <- D.fromKey "value" D.text c
    case u of
      "css selector"      -> pure $ ByCss (selectorFromText v)
      "link text"         -> pure $ LinkText v
      "partial link text" -> pure $ PartialLinkText v
      "tag name"          -> pure $ TagName v
      "xpath"             -> pure $ XPath v
      _                   -> throwing DE._ConversionFailure "LocateUsing"
