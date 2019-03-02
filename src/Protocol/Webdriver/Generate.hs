{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Generate
  ( createServantRoutes
  , createRequestTypes
  , createForest
  , parseWebdriverJSON
  , parseOrDie
  ) where

import           Control.Monad               ((>=>))
import           Control.Monad.IO.Class      (MonadIO, liftIO)

import           Control.Lens                (cons, ifoldMap, over, _head)

import qualified Data.ByteString             as BS

import qualified Data.Char                   as Char

import           Data.Text                   (Text)
import qualified Data.Text                   as T

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

alternate :: Int -> Text
alternate n = "\n" <> T.replicate n " " <> " :<|> "

combine :: Text
combine = " :> "

ucFirst :: Text -> Text
ucFirst = over _head Char.toUpper

ppRP :: RoutePiece -> Text
ppRP (Simple t)             = quote t
ppRP (Param (PathParam pp)) = "Capture " <> quote pp <> " Text"
ppRP (ReqBody cmdName _)    = "ReqBody '[WaargJSON WD] " <> ucFirst cmdName
ppRP (MethodTail m mr)      = methodText m <> " " <> maybe emptyType hasType mr
  where
    emptyType   = "'[] ()"
    hasType res = "'[WaargJSON WD] " <> typeishToHask (_respType res)

transformPath :: Route -> [RoutePiece]
transformPath = fmap mkPiece . filter (not . T.null) . T.splitOn "/" . _routeRaw
  where mkPiece p | T.isPrefixOf ":" p = Param . PathParam . T.tail $ p
                  | otherwise          = Simple p

createForest :: NonEmpty Route -> Tree.Forest RoutePiece
createForest = collapseForest . Tree.unfoldForest f . foldMap mkRoutePathTree
  where
    -- NER NER NONEMPTY LIST FOOL!
    f []    = error "the impossible happened (narrator: or did it?)"
    f [a]   = (a, [])
    f (a:t) = (a, [t])

    mkRoutePathTree :: Route -> [[RoutePiece]]
    mkRoutePathTree r = mappend (transformPath r) . mkMethod <$> _routeMethods r

    mkMethod :: RouteMethod -> [RoutePiece]
    mkMethod m = maybe mthd (:mthd) $ mkReqBody m where 
      mthd = [MethodTail (_routeMethod m) (_routeResp m)]

    mkReqBody :: RouteMethod -> Maybe RoutePiece
    mkReqBody m
      | null (_routeBody m) = Nothing
      | otherwise           = Just $ ReqBody (_routeCommand m) (_routeBody m)

collapseForest :: Tree.Forest RoutePiece -> Tree.Forest RoutePiece
collapseForest xs = if length roots < length xs then buildNewSubTree <$> roots else xs
  where
    roots = L.nub $ Tree.rootLabel <$> xs

    buildNewSubTree r = Tree.Node r 
      . collapseForest 
      $ collectSubForests r xs

    collectSubForests root = 
      foldMap (\t -> if Tree.rootLabel t == root then Tree.subForest t else mempty)

treeToString :: Int -> Tree.Tree RoutePiece -> Text
treeToString _   (Tree.Node root [])       = -- "GET '[Json] ()"
  ppRP root
treeToString lvl (Tree.Node root [child])  = -- "a" :> "b"
  ppRP root <> combine <> treeToString (lvl + 2) child
treeToString lvl (Tree.Node root children) = -- "a" :> ("b" :<|> "c" :<|> "d")
  ppRP root <> combine <> " (" <> spaces <> T.drop indent childRoutes <> spaces <> ")"
  where
    lvl'        = lvl + 2
    childRoutes = forestToString lvl' children
    indent      = T.length (alternate lvl')
    spaces      = cons '\n' $ T.replicate (indent - 1) " "

forestToString :: Int -> [Tree RoutePiece] -> Text
forestToString lvl = foldMap (mappend (alternate lvl) . treeToString lvl)

createServantRoutes :: Tree.Forest RoutePiece -> Text
createServantRoutes = (<> "\n") . mappend "type WebDriverApi = \"wd\" " . forestToString 2

createRequestTypes :: Tree.Forest RoutePiece -> Text
createRequestTypes = foldMap (foldMap onlyReqBody)
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
        recHeader <> ifoldMap (\i f -> pfx i <> toField f <> "\n") ps <> "  }\n"

    onlyReqBody (ReqBody c p) = toRecord c p
    onlyReqBody _             = mempty

parseWebdriverJSON :: MonadIO m => FilePath -> m (Either (DecodeError, CursorHistory) (NonEmpty Route))
parseWebdriverJSON = liftIO . BS.readFile >=> D.decodeFromByteString BS.parseOnly decodeRoutes

parseOrDie :: MonadIO m => FilePath -> m (NonEmpty Route)
parseOrDie = parseWebdriverJSON >=> either handleErr return
  where handleErr (e, h) = liftIO $
          putStr (show $ D.ppCursorHistory h) *>
          error (show e)
