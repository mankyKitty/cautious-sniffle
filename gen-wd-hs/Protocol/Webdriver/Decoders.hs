{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Decoders
  ( decodeRoutes
  ) where

import           Control.Monad            ((>=>))

import           Control.Lens             (snoc)
import           Control.Monad.Error.Lens (throwing)

import qualified Natural                  as Nat

import qualified Data.Text                as T

import           Data.List.NonEmpty       (NonEmpty ((:|)))

import           Waargonaut.Decode        (Decoder)
import qualified Waargonaut.Decode        as D
import           Waargonaut.Decode.Error  (_ConversionFailure)

import           Protocol.Webdriver.Types

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
    g "object"    = Just Objectly
    g "string"    = Just Stringly
    g "number"    = Just Numberly
    g "null"      = Just Nully
    g "boolean"   = Just Booleanly
    g "undefined" = Just Undefined
    g "*"         = Just LOL
    g _           = Nothing

    f t' | T.last t' == ']' = Array <$> f (T.take (T.length t' - 2) t')
         | T.head t' == '(' = Possibly <$> traverse g (T.splitOn "|" (T.init (T.tail t')))
         | otherwise        = g t'

decodeRouteMethod :: Monad f => Method -> Decoder f RouteMethod
decodeRouteMethod m = RouteMethod m
  <$> D.atKey "command" D.text
  <*> D.atKey "description" D.text
  <*> D.atKey "ref" D.text
  <*> D.atKey "parameters" (D.list decodeBodyParam)
  <*> D.try (D.atKey "returns" decodeResp)

stepNextKey :: Monad f => D.JCurs -> D.DecodeResult f D.JCurs
stepNextKey = D.moveRightN (Nat.successor' (Nat.successor' Nat.zero'))

getpathparams :: T.Text -> [PathParam]
getpathparams = fmap PathParam
  . filter (\p -> not (T.null p) && T.isPrefixOf ":" p )
  . T.splitOn "/"

decodeRoute :: Monad f => Decoder f Route
decodeRoute = D.withCursor $ \c -> do
  rawpath <- D.focus D.text c
  methodCurs <- D.moveRight1 c

  Route rawpath (getpathparams rawpath)
    <$> D.focus (D.passKeysToValues mempty decodeMethod decodeRouteMethod) methodCurs

decodeRoutes :: Monad f => Decoder f (NonEmpty Route)
decodeRoutes = D.withCursor $ D.down >=> \c -> (:|) 
  <$> D.focus decodeRoute c 
  <*> (stepNextKey c >>= D.foldCursor snoc stepNextKey mempty decodeRoute)
