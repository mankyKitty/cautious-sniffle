{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Generate.Streengs where

import           Control.Lens                         (cons, ifoldMap, over,
                                                       _head, _tail)

import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Data.Tree                            (Tree)
import qualified Data.Tree                            as Tree

import qualified Data.List                            as L
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NE

import           Protocol.Webdriver.Generate.Internal (overReqBody, ucFirst)
import           Protocol.Webdriver.Types

quote :: Text -> Text
quote t = "\"" <> t <> "\""

choice :: Text
choice = " :<|> "

alternate :: Int -> Text
alternate n = "\n" <> T.replicate n " " <> choice

combine :: Text
combine = " :> "

flattenedToString :: NonEmpty (NonEmpty RoutePiece) -> Text
flattenedToString =  mappend "\n"
  . mappend "type WebDriverApi = "
  . addChoice
  . addCombine
  . NE.toList
  . fmap (NE.toList . fmap ppRP)
  where
    fixHead = over _head ("\n       " <>)
    fixTail = over (_tail . traverse) (\a -> " " <> choice <> a)
    addChoice = T.unlines . fixHead . fixTail
    addCombine = fmap (mconcat . L.intersperse combine)

ppRP :: RoutePiece -> Text
ppRP (Simple t)             = quote t
ppRP (Param (PathParam pp)) = "Capture " <> quote pp <> " Text"
ppRP (ReqBody cmdName _)    = "ReqBody '[WaargJSON WDJson] " <> ucFirst cmdName
ppRP (MethodTail _ m mr)      = methodText m <> " " <> maybe emptyType hasType mr
  where
    handleNoContent "()" = "NoContent"
    handleNoContent typ  = typ

    emptyType   = "'[] NoContent"
    hasType res = "'[WaargJSON WDJson] " <> handleNoContent (typeishToHask (_respType res))

treeToString'
  :: (RoutePiece -> Text)
  -> Text
  -> Int
  -> Tree.Tree RoutePiece
  -> Text
treeToString' toText _ _   (Tree.Node root [])       = -- "GET '[Json] ()"
  toText root
treeToString' toText combiner lvl (Tree.Node root [child])  = -- "a" :> "b"
  toText root <> combiner <> treeToString' toText combiner (lvl + 2) child
treeToString' toText combiner lvl (Tree.Node root children) = -- "a" :> ("b" :<|> "c" :<|> "d")
  toText root <> combiner <> " (" <> spaces <> T.drop indent childRoutes <> spaces <> ")"
  where
    lvl'        = lvl + 2
    childRoutes = forestToString' toText combiner lvl' children
    indent      = T.length (alternate lvl')
    spaces      = cons '\n' $ T.replicate (indent - 1) " "

forestToString' :: (RoutePiece -> Text) -> Text -> Int -> [Tree RoutePiece] -> Text
forestToString' toText combiner lvl = foldMap (mappend (alternate lvl) . treeToString' toText combiner lvl)

treeToString :: Int -> Tree.Tree RoutePiece -> Text
treeToString = treeToString' ppRP combine

createServantRoutes :: Tree.Forest RoutePiece -> Text
createServantRoutes = mappend "\n"
  . mappend "type WebDriverApi = "
  . treeToString 2
  . Tree.Node (Simple "wd")

createClientFunctionImportList :: [Text] -> Text
createClientFunctionImportList = mconcat . addOpenParens . addCommas
  where
    addOpenParens = over _head ("  ( " <>)
    addCommas = over (_tail.traverse) ("\n  , " <>)

createRequestTypes :: Tree.Forest RoutePiece -> Text
createRequestTypes = (foldMap . foldMap) (overReqBody toRecord)
  where
    toRecord c ps =
      let
        n = ucFirst c

        fstPrefix = "    _"
        rstPrefix = "  , _"

        mkField fid typ = c <> fid <>  " :: " <> typ

        toMaybeType t | T.any (== ' ') t = "Maybe (" <> t <> ")"
                      | otherwise        = "Maybe " <> t

        mkType p = (if _bodyParamReqd p then id else toMaybeType)
          $ typeishToHask (_bodyParamType p)

        toField p = mkField (ucFirst $ _bodyParamName p) (mkType p)

        typetype = if length ps == 1
          then "newtype "
          else "data "

        recHeader = typetype <> n <> " = " <> n <> " {\n"

        pfx i | i == 0    = fstPrefix
              | otherwise = rstPrefix
      in
        recHeader
        <> ifoldMap (\i f -> pfx i <> toField f <> "\n") ps
        <> "  }\n"
        <> "  deriving (Show, Eq, GHC.Generic)\n\n"
        <> T.unlines
          [ "instance HasDatatypeInfo " <> n
          , "instance Generic " <> n
          , "instance JsonEncode WDJson " <> n
          , "instance JsonDecode WDJson " <> n
          ]
        <> "\n\n"

apiModule :: Text -> [Text] -> Text
apiModule api fns = T.unlines
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  , "module Protocol.Webdriver.ClientAPI"
  , createClientFunctionImportList fns
  , "  ) where"
  , mempty
  , "import Data.Proxy (Proxy (..))"
  , "import Data.Text (Text)"
  , "import Data.Vector (Vector)"
  , mempty
  , "import Servant.API"
  , "import Servant.Client"
  , "import Servant.API.ContentTypes.Waargonaut (WaargJSON)"
  , "import Waargonaut.Types.Json (Json)"
  , mempty
  , "import Protocol.Webdriver.ClientAPI.Types"
  , mempty
  , api
  , mempty
  , "webdriverAPI :: Proxy WebDriverApi"
  , "webdriverAPI = Proxy"
  , mempty
  , mconcat (L.intersperse "\n :<|> " fns) <> " = client webdriverAPI"
  ]

typesModule :: Text -> Text
typesModule types = T.unlines
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE FlexibleContexts #-}"
  , "module Protocol.Webdriver.ClientAPI.Types where"
  , mempty
  , "import qualified GHC.Generics as GHC"
  , mempty
  , "import Data.Text (Text)"
  , "import Data.Vector (Vector)"
  , "import qualified Data.Vector as V"
  , "import Data.Bool (Bool)"
  , "import Data.Scientific (Scientific)"
  , "import Waargonaut.Types.Json (Json)"
  , mempty
  , "import qualified Waargonaut.Encode as E"
  , "import qualified Waargonaut.Decode as D"
  , "import Waargonaut.Generic (Tagged (..), Generic, HasDatatypeInfo, JsonDecode (..), JsonEncode (..))"
  , mempty
  , "data WDJson"
  , mempty
  , "instance JsonEncode WDJson Json where mkEncoder = Tagged E.json"
  , "instance JsonDecode WDJson Json where mkDecoder = Tagged D.json"
  , "instance JsonEncode WDJson a => JsonEncode WDJson (Vector a) where mkEncoder = (\\e -> E.traversable e) <$> mkEncoder"
  , "instance JsonDecode WDJson a => JsonDecode WDJson (Vector a) where mkDecoder = (\\d -> D.withCursor (D.rightwardSnoc V.empty d)) <$> mkDecoder"
  , mempty
  , types
  ]
