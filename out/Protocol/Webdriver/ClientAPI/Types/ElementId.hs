{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Protocol.Webdriver.ClientAPI.Types.ElementId where

import           Data.Functor.Alt                            ((<!>))
import           Data.Text                                   (Text)

import           Servant.API                                 (ToHttpApiData (toUrlPiece))

import qualified Waargonaut.Decode                           as D
import           Waargonaut.Generic                          (JsonDecode (..))

import           Protocol.Webdriver.ClientAPI.Types.Internal (WDJson)

newtype ElementId = ElementId Text
  deriving (Show, Eq)

instance JsonDecode WDJson ElementId where
  mkDecoder = pure . fmap ElementId $
    D.atKey "ELEMENT" D.text <!>
    -- This seems wild and strange...
    D.atKey "element-6066-11e4-a52e-4f735466cecf" D.text

instance ToHttpApiData ElementId where
  toUrlPiece (ElementId eid) = eid
