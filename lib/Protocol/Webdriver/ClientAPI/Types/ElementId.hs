{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.ElementId where

import           Data.Functor.Alt                            ((<!>))
import           Data.Functor.Contravariant                  ((>$<))
import           Data.Text                                   (Text)

import           Servant.API                                 (ToHttpApiData (toUrlPiece))

import qualified Waargonaut.Decode                           as D
import qualified Waargonaut.Encode                           as E
import           Waargonaut.Generic                          (JsonDecode (..), JsonEncode (..))

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson, singleValueObj)

webElementIdentifier :: Text
webElementIdentifier = "element-6066-11e4-a52e-4f735466cecf"

newtype ElementId = ElementId Text
  deriving (Show, Eq)

decElementId :: Monad f => D.Decoder f ElementId
decElementId = fmap ElementId $
  D.atKey "ELEMENT" D.text <!>
  -- This seems wild and strange...
  D.atKey webElementIdentifier D.text

encElementId :: Applicative f => E.Encoder f ElementId
encElementId = (\(ElementId eid) -> eid) >$< E.text

encElementIdObject :: Applicative f => E.Encoder f ElementId
encElementIdObject = singleValueObj webElementIdentifier encElementId

instance JsonDecode WDJson ElementId where mkDecoder = pure decElementId
instance JsonEncode WDJson ElementId where mkEncoder = pure encElementId

instance ToHttpApiData ElementId where
  toUrlPiece (ElementId eid) = eid
