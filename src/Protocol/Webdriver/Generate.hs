{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Generate
  ( createClientFunctions
  , createForest
  , parseWebdriverJSON
  , parseOrDie
  , createFiles
  , apiFilename
  , typesFilename
  , flattenRoutes
  ) where

import           Control.Monad                        ((>=>))
import           Control.Monad.IO.Class               (MonadIO, liftIO)

import qualified Data.ByteString                      as BS
import           Data.Foldable                        (toList)

import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO

import qualified Data.Tree                            as Tree

import qualified Data.List                            as L
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE

import qualified Data.Attoparsec.ByteString           as BS

import           Waargonaut.Decode                    (CursorHistory)
import qualified Waargonaut.Decode                    as D
import           Waargonaut.Decode.Error              (DecodeError)

import           Protocol.Webdriver.Decoders          (decodeRoutes)
import           Protocol.Webdriver.Types

import qualified Language.Haskell.Exts                as HS
import qualified Protocol.Webdriver.Generate.Hask     as HGen
import           Protocol.Webdriver.Generate.Internal (onlyMT)

transformPath :: Route -> Maybe (NonEmpty RoutePiece)
transformPath = NE.nonEmpty . fmap mkPiece . filter (not . T.null) . T.splitOn "/" . _routeRaw
  where mkPiece p | T.isPrefixOf ":" p = Param . PathParam . T.tail $ p
                  | otherwise          = Simple p

transformRouteMethods :: RouteMethod -> NonEmpty RoutePiece
transformRouteMethods m = maybe mthd (<> mthd) $ mkReqBody m where
  mthd =  MethodTail (_routeCommand m) (_routeMethod m) (_routeResp m) :| []

  mkReqBody :: RouteMethod -> Maybe (NonEmpty RoutePiece)
  mkReqBody m' | null (_routeBody m') = Nothing
               | otherwise            = Just $ ReqBody (_routeCommand m') (_routeBody m') :| []

flattenRoute :: Route -> Maybe (NonEmpty (NonEmpty RoutePiece))
flattenRoute r = do
  rhead <- transformPath r
  rtails <- NE.nonEmpty $ transformRouteMethods <$> _routeMethods r
  pure $ (rhead <>) <$> rtails

flattenRoutes :: NonEmpty Route -> Maybe (NonEmpty (NonEmpty RoutePiece))
flattenRoutes = foldMap flattenRoute

createForest :: NonEmpty Route -> Tree.Forest RoutePiece
createForest = collapseForest . Tree.unfoldForest f . foldMap mkRoutePathTree
  where
    -- NER NER NONEMPTY LIST FOOL!
    f []    = error "the impossible happened (narrator: or did it?)"
    f [a]   = (a, [])
    f (a:t) = (a, [t])

    mkRoutePathTree :: Route -> [[RoutePiece]]
    mkRoutePathTree = maybe [] (NE.toList . fmap NE.toList) . flattenRoute

collapseForest :: Tree.Forest RoutePiece -> Tree.Forest RoutePiece
collapseForest xs = if length roots < length xs then buildNewSubTree <$> roots else xs
  where
    roots = L.nub $ Tree.rootLabel <$> xs

    buildNewSubTree r = Tree.Node r . collapseForest $ collectSubForests r xs

    collectSubForests root sf = flip foldMap sf $ \t ->
      if Tree.rootLabel t == root then Tree.subForest t else mempty

createClientFunctions :: Tree.Forest RoutePiece -> [Text]
createClientFunctions = fmap (onlyMT id mempty)
  . filter (onlyMT (const True) False)
  . foldMap toList

parseWebdriverJSON :: MonadIO m => FilePath -> m (Either (DecodeError, CursorHistory) (NonEmpty Route))
parseWebdriverJSON = liftIO . BS.readFile >=> D.decodeFromByteString BS.parseOnly decodeRoutes

parseOrDie :: MonadIO m => FilePath -> m (NonEmpty Route)
parseOrDie = parseWebdriverJSON >=> either handleErr return
  where handleErr (e, h) = liftIO $ putStr (show $ D.ppCursorHistory h) *> error (show e)

apiFilename :: FilePath
apiFilename = "ClientAPI.hs"

typesFilename :: FilePath
typesFilename = "Types.hs"

createFiles :: MonadIO m => FilePath -> FilePath -> m ()
createFiles inpFile destDir = liftIO $ do
  rs <- parseOrDie inpFile
  let rsf = createForest rs

  routes <- maybe (error "Dodgy route input") pure $ flattenRoutes rs
  fns <- maybe (error "Dodgy function list") pure . NE.nonEmpty $ createClientFunctions rsf

  let
    apiMod = HGen.apiModule (HGen.createServantAPIType routes) fns []
    typeMod = HGen.typesModule rsf

    pphask = T.pack . HS.prettyPrintStyleMode
      (HS.Style HS.PageMode 100 2.0)
      (HS.PPHsMode 2 2 2 2 2 2 1 True HS.PPOffsideRule False)

  TIO.writeFile (destDir <> "/" <> apiFilename) (pphask apiMod)
  TIO.writeFile (destDir <> "/ClientAPI/" <> typesFilename) (pphask typeMod)

  --   api = fold $ flattenedToString <$> flattenRoutes rs
  --   fns = createClientFunctions rsf
  --   types = createRequestTypes rsf

  -- TIO.writeFile (destDir <> "/" <> apiFilename) (apiModule api fns)
  -- TIO.writeFile (destDir <> "/ClientAPI/" <> typesFilename) (typesModule types)
