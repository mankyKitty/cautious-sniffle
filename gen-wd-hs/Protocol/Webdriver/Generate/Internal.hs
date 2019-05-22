module Protocol.Webdriver.Generate.Internal where

import           Control.Lens             (over, _head)
import qualified Data.Char                as Char

import           Data.Text                (Text)

import           Protocol.Webdriver.Types

overReqBody :: Monoid m => (Text -> [BodyParam] -> m) -> RoutePiece -> m
overReqBody f (ReqBody c p) = f c p
overReqBody _ _             = mempty

onlyMT :: (Text -> p) -> p -> RoutePiece -> p
onlyMT f _ (MethodTail cmd _ _) = f cmd
onlyMT _ g _                    = g

ucFirst :: Text -> Text
ucFirst = over _head Char.toUpper

