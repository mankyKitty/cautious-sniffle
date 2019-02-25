{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Protocol.Webdriver.Generate where

import           Control.Monad              ((<=<), (>=>))
import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Control.Lens               (cons, ifoldr, imap, (^.), (^?),
                                             _head, _last)
import           Control.Monad.Error.Lens   (throwing)

import qualified Natural                    as Nat

import Data.Maybe (fromMaybe)
import qualified Data.ByteString            as BS

import           Data.GenericTrie           (Trie)
import qualified Data.GenericTrie           as GT

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Data.Attoparsec.ByteString as BS

import qualified Waargonaut                 as W
import           Waargonaut.Decode          (CursorHistory, Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError, _ConversionFailure)

import           Algebra.Graph              (Graph)
import qualified Algebra.Graph              as G

-- import           Algebra.Graph.AdjacencyMap              (AdjacencyMap)
-- import qualified Algebra.Graph.AdjacencyMap             as G

-- General Route Definition Structure:
--
-- "path/:parameter" :{
--   "METHOD": {
--     "command": "functionName",
--     "description": "Short help text"
--     "ref": "link to relevant spec"
--     "parameters": [ -- body content for the request
--       { "name": "param name"
--       , "type": "param type"
--       , "description": "brief description"
--       , "required": "bool"
--       }],
--     "returns": {
--       "type": "method response",
--       "name": "response type",
--       "description": "description of response content",
--     }
--   }
-- }
--
-- Example:
--
-- @
-- "/session": {
--   "POST": {
--     "command": "newSession",
--     "description": "The New Session command creates a new WebDriver session with the endpoint node. If the creation fails, a session not created error is returned.",
--     "ref": "https://w3c.github.io/webdriver/#dfn-new-sessions",
--     "parameters": [{
--       "name": "capabilities",
--       "type": "object",
--       "description": "a JSON object, the set of capabilities that was ultimately merged and matched in the capability processing algorithm",
--       "required": true
--     }],
--     "returns": {
--       "type": "Object",
--       "name": "session",
--       "description": "Object containing sessionId and capabilities of created WebDriver session."
--     }
--   }
 -- },
--
-- @

data Method
  = GET
  | POST
  | DELETE
  deriving (Show, Eq, Ord)

data Typeish
  = Objectly
  | Stringly
  | Numberly
  | Nully
  | Booleanly
  | Lol
  | Array Typeish
  | Possibly [Typeish]
  deriving (Show, Eq, Ord)

newtype PathParam = PathParam Text
  deriving (Show, Eq, Ord)

data BodyParam = BodyParam
  { _bodyParamName :: Text
  , _bodyParamType :: Typeish
  , _bodyParamDesc :: Text
  , _bodyParamReqd :: Bool
  }
  deriving (Show, Eq)

data Resp = Resp
  { _respType :: Typeish
  , _respName :: Text
  , _respDesc :: Text
  }
  deriving (Show, Eq, Ord)

data RouteMethod = RouteMethod
  { _routeMethod  :: Method
  , _routeCommand :: Text
  , _routeDesc    :: Text
  , _routeRef     :: Text
  , _routeBody    :: [BodyParam]
  , _routeResp    :: Maybe Resp
  }
  deriving (Show, Eq)

data Route = Route
  { _routeRaw     :: Text
  , _routeParams  :: [PathParam]
  , _routeMethods :: [RouteMethod]
  }
  deriving (Show, Eq)

data RoutePiece
  = Simple Text
  | Param PathParam
  | MethodTail Method (Maybe Resp)
  deriving (Show, Eq, Ord)

newtype RP = RP (Int, RoutePiece)
  deriving Eq

instance Show RP where
  show (RP (_, rp)) = ppRP rp

instance Ord RP where
  compare (RP (a,_)) (RP (b,_)) = compare a b

ppGraph :: Graph RP -> Text
ppGraph = G.foldg "root" (T.pack . show) (\a b -> a <> "\n  :<|> " <> b <> "\n") (\a b -> a <> " :> " <> b)

ppRP :: RoutePiece -> String
ppRP (Simple t)             = T.unpack t
ppRP (Param (PathParam pp)) = "Capture " <> T.unpack pp <> " Text"
ppRP (MethodTail m mr)      = show m <> " " <> maybe "'[] ()" (\res -> "'[JSON] " <>  title (_respName res)) mr
  where
    title = T.unpack . T.toTitle

transformPath :: Route -> [RoutePiece]
transformPath = fmap mkPiece . filter (not . T.null) . T.splitOn "/" . _routeRaw
  where mkPiece p | T.isPrefixOf ":" p = Param (PathParam p)
                  | otherwise          = Simple p

decodeBodyParam :: Monad f => Decoder f BodyParam
decodeBodyParam = BodyParam
  <$> D.atKey "name" D.text
  <*> D.atKey "type" decodeTypeish
  <*> D.atKey "description" D.text
  <*> D.atKey "required" D.bool

decodeResp :: Monad f => Decoder f Resp
decodeResp = Resp
  <$> D.atKey "type" decodeTypeish
  <*> D.atKey "name" D.text
  <*> D.atKey "description" D.text

decodeMethod :: Monad f => Decoder f Method
decodeMethod = (\i -> maybe (throwing _ConversionFailure i) pure . m . T.toUpper $ i) =<< D.text
  where
    m "POST"   = Just POST
    m "DELETE" = Just DELETE
    m "GET"    = Just GET
    m _        = Nothing

decodeTypeish :: Monad f => Decoder f Typeish
decodeTypeish = (\i -> maybe (throwing _ConversionFailure i) pure . f . T.toLower $ i) =<< D.text
  where
    g "object"  = Just Objectly
    g "string"  = Just Stringly
    g "number"  = Just Numberly
    g "null"    = Just Nully
    g "boolean" = Just Booleanly
    g "*"       = Just Lol
    g _         = Nothing

    f t' | T.last t' == ']' = Array <$> f (T.take (T.length t' - 2) t')
         | T.head t' == '(' = Possibly <$> traverse g (T.splitOn "|" (T.init (T.tail t')))
         | otherwise        = g t'

decodeRouteMethod :: Monad f => Decoder f RouteMethod
decodeRouteMethod = D.withCursor $ \c -> do
  m <- D.focus decodeMethod c
  mobj <- D.moveRight1 c >>= D.down

  RouteMethod m
    <$> D.fromKey "command" D.text mobj
    <*> D.fromKey "description" D.text mobj
    <*> D.fromKey "ref" D.text mobj
    <*> D.fromKey "parameters" (D.list decodeBodyParam) mobj
    <*> D.try (D.fromKey "returns" decodeResp mobj)

decodeRoute :: Monad f => Decoder f Route
decodeRoute = D.withCursor $ \c -> do
  rawpath <- D.focus D.text c
  methodCurs <- D.moveRight1 c >>= D.down

  Route rawpath (getpathparams rawpath) <$>
    D.foldCursor
    (flip cons)
    (D.moveRightN (Nat.successor' (Nat.successor' Nat.zero')))
    mempty
    decodeRouteMethod
    methodCurs

  where
    getpathparams = fmap PathParam
      . filter (\p -> not (T.null p) && T.isPrefixOf ":" p )
      . T.splitOn "/"

decodeRoutes :: Monad f => Decoder f [Route]
decodeRoutes = D.withCursor $ D.down >=> D.foldCursor (flip cons)
  (D.moveRightN (Nat.successor' (Nat.successor' Nat.zero')))
  mempty
  decodeRoute

parseWebdriverJSON :: MonadIO m => FilePath -> m (Either (DecodeError, CursorHistory) [Route])
parseWebdriverJSON = D.runDecode decodeRoutes (D.parseWith BS.parseOnly W.parseWaargonaut)
  . D.mkCursor <=< liftIO . BS.readFile
