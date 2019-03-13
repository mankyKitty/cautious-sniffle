{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Generate
  ( createServantRoutes
  , createRequestTypes
  , createClientFunctions
  , createForest
  , parseWebdriverJSON
  , parseOrDie
  , createFiles
  , apiModule
  , typesModule
  , apiFilename
  , typesFilename
  , flattenRoutes
  ) where

import           Control.Monad               ((>=>))
import           Control.Monad.IO.Class      (MonadIO, liftIO)

import           Control.Lens                (cons, ifoldMap, over, _head, _tail)

import qualified Data.ByteString             as BS
import           Data.Foldable               (fold, toList)

import qualified Data.Char                   as Char

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

import           Data.Tree                   (Tree)
import qualified Data.Tree                   as Tree

import qualified Data.List                   as L
import           Data.List.NonEmpty          (NonEmpty)

import qualified Data.Attoparsec.ByteString  as BS

import           Waargonaut.Decode           (CursorHistory)
import qualified Waargonaut.Decode           as D
import           Waargonaut.Decode.Error     (DecodeError)

import           Protocol.Webdriver.Decoders (decodeRoutes)
import           Protocol.Webdriver.Types

quote :: Text -> Text
quote t = "\"" <> t <> "\""

choice :: Text
choice = " :<|> "

alternate :: Int -> Text
alternate n = "\n" <> T.replicate n " " <> choice

combine :: Text
combine = " :> "

ucFirst :: Text -> Text
ucFirst = over _head Char.toUpper

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

transformPath :: Route -> [RoutePiece]
transformPath = fmap mkPiece . filter (not . T.null) . T.splitOn "/" . _routeRaw
  where mkPiece p | T.isPrefixOf ":" p = Param . PathParam . T.tail $ p
                  | otherwise          = Simple p

transformRouteMethods :: RouteMethod -> [RoutePiece]
transformRouteMethods m = fold (mkReqBody m) <> mthd where
  mthd = [MethodTail (_routeCommand m) (_routeMethod m) (_routeResp m)]

  mkReqBody :: RouteMethod -> Maybe [RoutePiece]
  mkReqBody m' | null (_routeBody m') = Nothing
               | otherwise           = Just $ [ReqBody (_routeCommand m') (_routeBody m')]

flattenRoutes :: NonEmpty Route -> [[RoutePiece]]
flattenRoutes = foldMap flattenRoute
  where
    flattenRoute r =
      let
        rhead = transformPath r
        rtails = transformRouteMethods <$> _routeMethods r
      in
        mappend rhead <$> rtails

flattenedToString :: [[RoutePiece]] -> Text
flattenedToString =  mappend "\n"
  . mappend "type WebDriverApi = " 
  . addChoice . addCombine . (fmap . fmap) ppRP
  where
    fixHead = over _head ("\n       " <>)
    fixTail = over (_tail . traverse) (\a -> " " <> choice <> a)
    addChoice = T.unlines . fixHead . fixTail
    addCombine = fmap (mconcat . L.intersperse combine)

createForest :: NonEmpty Route -> Tree.Forest RoutePiece
createForest = collapseForest . Tree.unfoldForest f . foldMap mkRoutePathTree
  where
    -- NER NER NONEMPTY LIST FOOL!
    f []    = error "the impossible happened (narrator: or did it?)"
    f [a]   = (a, [])
    f (a:t) = (a, [t])

    mkRoutePathTree :: Route -> [[RoutePiece]]
    mkRoutePathTree r = mappend (transformPath r) . transformRouteMethods
      <$> _routeMethods r

collapseForest :: Tree.Forest RoutePiece -> Tree.Forest RoutePiece
collapseForest xs = if length roots < length xs then buildNewSubTree <$> roots else xs
  where
    roots = L.nub $ Tree.rootLabel <$> xs

    buildNewSubTree r = Tree.Node r
      . collapseForest
      $ collectSubForests r xs

    collectSubForests root sf = flip foldMap sf $ \t ->
      if Tree.rootLabel t == root then Tree.subForest t else mempty

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

overReqBody :: Monoid m => (Text -> [BodyParam] -> m) -> RoutePiece -> m
overReqBody f (ReqBody c p) = f c p
overReqBody _ _             = mempty

onlyMT :: (Text -> p) -> p -> RoutePiece -> p
onlyMT f _ (MethodTail cmd _ _) = f cmd
onlyMT _ g _                    = g

createClientFunctions :: Tree.Forest RoutePiece -> [Text]
createClientFunctions = fmap (onlyMT id mempty)
  . filter (onlyMT (const True) False)
  . foldMap toList

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

parseWebdriverJSON :: MonadIO m => FilePath -> m (Either (DecodeError, CursorHistory) (NonEmpty Route))
parseWebdriverJSON = liftIO . BS.readFile >=> D.decodeFromByteString BS.parseOnly decodeRoutes

parseOrDie :: MonadIO m => FilePath -> m (NonEmpty Route)
parseOrDie = parseWebdriverJSON >=> either handleErr return
  where handleErr (e, h) = liftIO $
          putStr (show $ D.ppCursorHistory h) *>
          error (show e)

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

apiFilename :: FilePath
apiFilename = "ClientAPI.hs"

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

typesFilename :: FilePath
typesFilename = "Types.hs"

createFiles :: MonadIO m => FilePath -> FilePath -> m ()
createFiles inpFile destDir = liftIO $ do
  rs <- parseOrDie inpFile
  let
    rsf = createForest rs

    api = flattenedToString (flattenRoutes rs)
    fns = createClientFunctions rsf
    types = createRequestTypes rsf

  TIO.writeFile (destDir <> "/" <> apiFilename) (apiModule api fns)
  TIO.writeFile (destDir <> "/ClientAPI/" <> typesFilename) (typesModule types)
