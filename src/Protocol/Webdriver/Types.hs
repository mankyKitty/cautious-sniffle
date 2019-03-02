{-# LANGUAGE OverloadedStrings #-}
module Protocol.Webdriver.Types
  ( WD
  , Method (..)
  , Typeish (..)
  , PathParam (..)
  , BodyParam (..)
  , Resp (..)
  , RouteMethod (..)
  , Route (..)
  , RoutePiece (..)
  , methodText
  , typeishToHask
  ) where

import Data.Text (Text)

data WD

data Method
  = GET
  | POST
  | DELETE
  deriving (Show, Eq, Ord)

methodText :: Method -> Text
methodText GET    = "GET"
methodText POST   = "POST"
methodText DELETE = "DELETE"

data Typeish
  = Objectly
  | Stringly
  | Numberly
  | Nully
  | Undefined
  | Booleanly
  | LOL
  | Array Typeish
  | Possibly [Typeish]
  deriving (Show, Eq, Ord)

typeishToHask :: Typeish -> Text
-- I guess?
typeishToHask Objectly = "Json"
typeishToHask LOL      = "Json"
typeishToHask (Possibly _) = "Json"
-- sigh...
typeishToHask Nully    = "()"
typeishToHask Undefined = "()"
-- woo
typeishToHask Stringly = "Text"
typeishToHask Numberly = "Scientific"
typeishToHask Booleanly = "Bool"
typeishToHask (Array t) = "Vector " <> typeishToHask t

newtype PathParam = PathParam Text
  deriving (Show, Eq, Ord)

data BodyParam = BodyParam
  { _bodyParamName :: Text
  , _bodyParamType :: Typeish
  , _bodyParamDesc :: Text
  , _bodyParamReqd :: Bool
  }
  deriving (Show, Ord, Eq)

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
  | ReqBody Text [BodyParam]
  deriving (Show, Eq, Ord)
