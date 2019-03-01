{-# LANGUAGE OverloadedStrings #-}

module Protocol.Webdriver.Generate where

import           Control.Monad              ((<=<), (>=>))
import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Control.Lens               (snoc)
import           Control.Monad.Error.Lens   (throwing)

import qualified Natural                    as Nat

import qualified Data.ByteString            as BS

import           Data.Foldable              (fold)

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Tree                  (Tree)
import qualified Data.Tree                  as Tree

import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty ((:|)))

import qualified Data.Attoparsec.ByteString as BS

import qualified Waargonaut                 as W
import           Waargonaut.Decode          (CursorHistory, Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError, _ConversionFailure)

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

-- typeishToHaskell :: Typeish -> String
-- typeishToHaskell Objectly       = "(Map Text Text)"
-- typeishToHaskell Stringly       = "Text"
-- typeishToHaskell Numberly       = "Int"
-- typeishToHaskell Nully          = "()"
-- typeishToHaskell Booleanly      = "Bool"
-- typeishToHaskell Lol            = "Text"
-- typeishToHaskell (Array t)      = "["<> typeishToHaskell t <> "]"
-- typeishToHaskell (Possibly [])  = "()"
-- typeishToHaskell (Possibly [a]) = "(Maybe " <> typeishToHaskell a  <> ")"
-- typeishToHaskell (Possibly ts)   = "[" <> fold (L.intersperse "," (typeishToHaskell <$> ts)) <> "]"

ppRP :: RoutePiece -> String
ppRP (Simple t)             = "\"" <> T.unpack t <> "\""
ppRP (Param (PathParam pp)) = "Capture " <> "\"" <> T.unpack pp <> "\"" <> " Text"
ppRP (MethodTail m mr)      = show m <> " "
  <> maybe "'[] ()" (\res -> "'[WaargJSON WD] " <> title (_respName res)) mr
  where
    title = T.unpack . T.toTitle

transformPath :: Route -> [RoutePiece]
transformPath = fmap mkPiece . filter (not . T.null) . T.splitOn "/" . _routeRaw
  where mkPiece p | T.isPrefixOf ":" p = Param (PathParam p)
                  | otherwise          = Simple p

createForest :: NonEmpty Route -> Tree.Forest RoutePiece
createForest = Tree.unfoldForest f . foldMap mkRoutePathTree
  where
    -- NER NER NONEMPTY LIST FOOL!
    f [a]   = (a, [])
    f (a:t) = (a, [t])

collapseForest :: Tree.Forest RoutePiece -> Tree.Forest RoutePiece
collapseForest xs = if length roots < length xs
  then (\r -> Tree.Node r . collapseForest $ collectSubForests r xs) <$> roots
  else xs
  where
    roots                  = L.nub . fmap Tree.rootLabel $ xs
    collectSubForests root = foldMap Tree.subForest . filter ((== root) . Tree.rootLabel)

alternate :: Int -> String
alternate n = "\n" <> replicate n ' ' <> " :<|> "

combine :: String
combine = " :> "

treeToString :: Int -> Tree.Tree RoutePiece -> String
treeToString _   (Tree.Node root [])       =
  -- "GET '[Json] ()"
  ppRP root
treeToString lvl (Tree.Node root [child])  =
  -- "a" :> "b"
  ppRP root <> combine <> treeToString (lvl + 2) child
treeToString lvl (Tree.Node root children) =
  -- "a" :> ("b" :<|> "c" :<|> "d")
  ppRP root <> combine <> " (" <> spaces <> L.drop indent childRoutes <> spaces <> ")"
  where
    lvl' = lvl + 2
    childRoutes = forestToString lvl' children
    indent = length (alternate lvl')
    spaces = '\n':replicate (indent - 7) ' '

forestToString :: Int -> [Tree RoutePiece] -> String
forestToString lvl = foldMap (mappend (alternate lvl) . treeToString lvl )

createServantRoutes :: Tree.Forest RoutePiece -> String
createServantRoutes = (<> "\n") . mappend "root " . forestToString 2

wretched :: IO (Tree.Forest RoutePiece)
wretched = do
  Right r <- parseWebdriverJSON "protocol/webdriver.json"
  pure $ collapseForest (createForest r)

goDo :: IO ()
goDo = do
  Right r <- parseWebdriverJSON "protocol/webdriver.json"
  let mooshedForest = collapseForest (createForest r)
  putStr . Tree.drawForest $ (fmap . fmap) ppRP mooshedForest

mkRoutePathTree :: Route -> [[RoutePiece]]
mkRoutePathTree r = snoc (transformPath r) <$> mkMths (_routeMethods r)
  where mkMths  = fmap (\m -> MethodTail (_routeMethod m) (_routeResp m))

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

  Route rawpath (getpathparams rawpath) <$> D.foldCursor snoc
    (D.moveRightN (Nat.successor' (Nat.successor' Nat.zero')))
    mempty
    decodeRouteMethod
    methodCurs

  where
    getpathparams = fmap PathParam
      . filter (\p -> not (T.null p) && T.isPrefixOf ":" p )
      . T.splitOn "/"

decodeRoutes :: Monad f => Decoder f (NonEmpty Route)
decodeRoutes = D.withCursor $ D.down >=> \c ->
  (:|) <$> D.focus decodeRoute c <*> (step c >>= D.foldCursor snoc step mempty decodeRoute)
  where step = D.moveRightN (Nat.successor' (Nat.successor' Nat.zero'))

parseWebdriverJSON :: MonadIO m => FilePath -> m (Either (DecodeError, CursorHistory) (NonEmpty Route))
parseWebdriverJSON = D.runDecode decodeRoutes (D.parseWith BS.parseOnly W.parseWaargonaut)
  . D.mkCursor <=< liftIO . BS.readFile
