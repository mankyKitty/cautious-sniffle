{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Represents the unique identifier for an element in the DOM on the page.
--
module Protocol.Webdriver.ClientAPI.Types.ElementId
  ( -- * Types
    ElementId (..)
    -- * Helpers
  , decElementId
  , encElementId
  , encElementIdObject

  ) where

import           Data.Functor.Alt                            ((<!>))
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Text                                   (Text)

import           Servant.API                                 (ToHttpApiData (toUrlPiece))

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..), JsonEncode (..))

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson, singleValueObj)

-- | Unique key that points to the value that is the actual element ID on JSON structures.
--
-- [spec](https://w3c.github.io/webdriver/#elements)
--
webElementIdentifier :: Text
webElementIdentifier =
  -- Your guess is as good as mine for why it is this way. This could
  -- be used internally in the drivers for versioning? But there is no
  -- way to establish what this needs to be if it has changed. So
  -- we'll get runtime failures later, yay.
  "element-6066-11e4-a52e-4f735466cecf"

newtype ElementId = ElementId Text
  deriving (Show, Eq)

decElementId :: Monad f => D.Decoder f ElementId
decElementId = fmap ElementId $
  -- The actual element identifier will be located at one of the following keys:
  D.atKey "ELEMENT" D.text <!> D.atKey webElementIdentifier D.text

-- | Encodes the element id itself, without the wrapper component.
encElementId :: Applicative f => E.Encoder f ElementId
encElementId = (\(ElementId eid) -> eid) >$< E.text

-- | Encodes the element id in a wrapper object with the 'webElementIdentifier' as the key.
encElementIdObject :: Applicative f => E.Encoder f ElementId
encElementIdObject = singleValueObj webElementIdentifier encElementId

instance JsonDecode WDJson ElementId where mkDecoder = pure decElementId
instance JsonEncode WDJson ElementId where mkEncoder = pure encElementId

instance ToHttpApiData ElementId where
  toUrlPiece (ElementId eid) = eid
