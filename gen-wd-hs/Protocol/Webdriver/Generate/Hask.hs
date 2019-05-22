{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
module Protocol.Webdriver.Generate.Hask
  ( createRequestTypeDecls
  , createClientAPIFunctions
  , createClientAPIFunctionsDoc
  , createServantAPIType
  , createApiProxy
  , rpToHask
  , apiModule
  , typesModule
  , vecEncInstance
  , vecDecInstance
  , jsonInstance
  , createServantAPIDoc
  , apiDoc
  ) where

import           Control.Lens                           (over, (^?), _last)
import           Data.Distributive                      (distribute)

import qualified Data.Tree                              as Tree

import           Data.Text                              (Text)
import qualified Data.Text                              as T

import qualified Data.List as L
import           Data.List.NonEmpty                     (NonEmpty (..))

import           Data.Foldable                          (foldr1, toList)
import           Protocol.Webdriver.Types

import           Text.PrettyPrint                       (Doc)
import qualified Text.PrettyPrint                       as PP

import qualified Language.Haskell.Exts                  as HS
import qualified Language.Haskell.TH.LanguageExtensions as HSLE

import           Protocol.Webdriver.Generate.Internal   (overReqBody, ucFirst)

import qualified Protocol.Webdriver.HaskGen             as HGen

waargTag :: String
waargTag = "WDJson"

promotedWaargJson :: HS.Type ()
promotedWaargJson =
  let
    wjwd = HS.TyApp () (HGen.simpleTyDecl "WaargJSON") (HGen.simpleTyDecl waargTag)
  in
    HS.TyPromoted () (HS.PromotedList () True [wjwd])

rpToHask :: RoutePiece -> HS.Type ()
rpToHask (Simple t) =
  HS.TyPromoted () $ HGen.promotedString (T.unpack t)
rpToHask (Param (PathParam pp)) =
  let
    cap   = HGen.simpleTyDecl "Capture"
    typ   = HGen.simpleTyDecl "Text"
    ppstr = HGen.promotedString (T.unpack pp)
  in
    HS.TyApp () (HS.TyApp () cap (HS.TyPromoted () ppstr)) typ

rpToHask (ReqBody cmdName _) =
  let
    req  = HGen.simpleTyDecl "ReqBody"
    cmd  = HGen.simpleTyDecl (T.unpack $ ucFirst cmdName)
    pwjd = promotedWaargJson
  in
    HS.TyApp () (HS.TyApp () req pwjd) cmd

rpToHask (MethodTail _ m mr) =
  let
    emptyPromotedList = HS.TyPromoted () (HS.PromotedList () True [])
    emptyResponse     = (emptyPromotedList,HGen.simpleTyDecl "NoContent")
    mthd              = HGen.simpleTyDecl (T.unpack . T.toTitle . T.pack . show $ m)

    (cty, rty) = case mr of
      Nothing -> emptyResponse
      Just res -> if _respType res `elem` [Nully, Undefined]
        then emptyResponse
        else (promotedWaargJson, HGen.toTypeDecl (_respType res))
  in
    HS.TyApp () (HS.TyApp () mthd cty) rty

apiUnion :: String
apiUnion = ":<|>"

apiContained :: String
apiContained = ":>"

alternateSymbol :: HS.QName ()
alternateSymbol = HS.UnQual () $ HS.sym apiUnion

mkInfixTyOp :: HS.QName () -> HS.Type () -> HS.Type () -> HS.Type()
mkInfixTyOp op a = HS.TyInfix () a (HS.UnpromotedName () op)

rpath :: HS.Type () -> HS.Type () -> HS.Type()
rpath = mkInfixTyOp (HS.UnQual () $ HS.sym apiContained)

ralt :: HS.Type () -> HS.Type () -> HS.Type()
ralt = mkInfixTyOp alternateSymbol

ppNewline :: Doc
ppNewline = PP.text "\n"

createServantAPIDoc :: NonEmpty (NonEmpty RoutePiece) -> Doc
createServantAPIDoc xs =
  let
    pline = HS.prettyPrintStyleMode (PP.style { PP.mode = PP.OneLineMode }) HS.defaultMode
    apilines = fmap (HS.TyParen () . foldr1 rpath . fmap rpToHask) xs
    thead = PP.text $ "type " <> apiTypeName <> " = "
    leadsym = PP.text $ "  " <> apiUnion <> " "

    todoc (h :| t) =
      PP.text "       " <> PP.text (pline h)
      : fmap (mappend leadsym . PP.text . pline) t

  in
    PP.vcat $ thead : todoc apilines

createClientAPIFunctionsDoc :: NonEmpty Text -> Doc
createClientAPIFunctionsDoc (a :| xs) =
  let
    un = 
      PP.space <> PP.text apiUnion <> PP.space

    fn = 
      PP.text . T.unpack

    mkLast l = 
      un <> fn l <> PP.space <> PP.text " = client " <> PP.text apiProxyName

    lastFn = 
      PP.space <> maybe PP.empty mkLast (xs ^? _last)

    fns = 
      PP.vcat . over _last (<> lastFn) $ mappend un . fn <$> init xs
  in
    PP.vcat
      [ fn a
      , PP.nest 2 fns
      ]

createServantAPIType :: NonEmpty (NonEmpty RoutePiece) -> HS.Decl ()
createServantAPIType = HS.TypeDecl () (HS.DHead () $ HS.name apiTypeName)
  . foldr1 ralt . fmap (HS.TyParen () . foldr1 rpath . fmap rpToHask)

createClientAPIFunctions :: NonEmpty Text -> HS.Decl ()
createClientAPIFunctions (a :| xs) = HS.FunBind () . pure $ HS.InfixMatch ()
  patHead
  (HS.sym apiUnion)
  [foldr1 fnInfix $ patName <$> xs]
  (HS.UnGuardedRhs () (HS.App () (HS.Var () $ HGen.unqname "client") (HS.Var () $ HGen.unqname apiProxyName)))
  Nothing
  where
    patHead = patName a
    patName = HS.PVar () . HS.name . T.unpack
    fnInfix l = HS.PInfixApp () l alternateSymbol

apiTypeName :: String
apiTypeName = "WebDriverAPI"

apiProxyName :: String
apiProxyName = "webdriverApi"

createApiProxy :: [HS.Decl ()]
createApiProxy =
  [ HS.TypeSig () [apiname] (HS.TyApp () (HGen.simpleTyDecl "Proxy") (HGen.simpleTyDecl apiTypeName))
  , HS.FunBind () [HS.Match () apiname [] (HS.UnGuardedRhs () . HS.Var () $ HGen.unqname "Proxy") Nothing]
  ]
  where
    apiname = HS.name apiProxyName

onety :: Text -> String -> HS.Decl ()
onety c ty = HGen.instanceOneTy
  (HGen.unqname (T.unpack c))
  $ HGen.simpleTyDecl ty

twoty :: Text -> String -> String -> HS.Decl ()
twoty c tyA tyB = HGen.instanceTwoTy
  (HGen.unqname (T.unpack c))
  (HGen.simpleTyDecl tyA)
  (HGen.simpleTyDecl tyB)

mkInstances :: String -> [HS.Decl ()]
mkInstances = distribute
  [ onety "HasDatatypeInfo"
  , onety "Generic"
  , twoty "JsonEncode" "WDJson"
  , twoty "JsonDecode" "WDJson"
  ]

mkDerivings :: HS.Deriving ()
mkDerivings = HGen.mkDeriving
  [ HGen.unqname "Show"
  , HGen.unqname "Eq"
  , HGen.qname (HGen.moduleName "GHC") "Generic"
  ]

createRequestTypeDecls :: Tree.Forest RoutePiece -> [HS.Decl ()]
createRequestTypeDecls = (foldMap . foldMap) (overReqBody typeAndInstances)
  where
    typeAndInstances c ps =
      let typeName = T.unpack (ucFirst c)
      in createRecord c typeName ps : mkInstances typeName

    toField fld p = (T.unpack $ fld <> ucFirst (_bodyParamName p),)
      . (if _bodyParamReqd p then id else HGen.wrapMaybe)
      $ HGen.toTypeDecl (_bodyParamType p)

    createRecord fld c [a] = -- Make a newtype
      HGen.mkNewType (HS.name c) ("un" <> c) (snd $ toField fld a) mkDerivings
    createRecord fld c ps = -- Make a normal Record data type
      HGen.mkRecord (HS.name c) (toField fld <$> ps) mkDerivings

textTypeImport :: HS.ImportDecl ()
textTypeImport = HGen.mkImportDecl "Data.Text" [HGen.importOnly "Text"]

vectorTypeImport :: HS.ImportDecl ()
vectorTypeImport = HGen.mkImportDecl "Data.Vector" [HGen.importOnly "Vector"]

jsonTypeImport :: HS.ImportDecl ()
jsonTypeImport = HGen.mkImportDecl "Waargonaut.Types.Json" [HGen.importOnly "Json"]

apiModuleImports :: [HS.ImportDecl ()]
apiModuleImports =
  [ HGen.mkImportDecl "Data.Proxy" [HGen.importAllOf "Proxy"]
  , textTypeImport
  , vectorTypeImport
  , jsonTypeImport
  , HGen.importEntire "Servant.API"
  , HGen.importEntire "Servant.Client"
  , HGen.mkImportDecl "Servant.API.ContentTypes.Waargonaut" [HGen.importOnly "WaargJSON"]
  , HGen.importEntire "Protocol.Webdriver.ClientAPI.Types"
  ]

apiDoc :: NonEmpty (NonEmpty RoutePiece) -> NonEmpty Text -> Doc
apiDoc api fns =
  let
    rr = PP.text . HS.prettyPrint
    rrs = PP.vcat . fmap rr
  in
    PP.vcat $ L.intersperse ppNewline
      [ rrs $ HGen.mkPragmas [HSLE.DataKinds, HSLE.TypeOperators]
      , rr $ HS.OptionsPragma () (Just HS.GHC) "-Wno-missing-signatures"
      , rr $ HGen.mkModuleHead "Protocol.Webdriver.ClientAPI" Nothing (toList $ T.unpack <$> fns)
      , rrs apiModuleImports
      , createServantAPIDoc api
      , rrs createApiProxy
      , createClientAPIFunctionsDoc fns
      ]

apiModule :: HS.Decl () -> NonEmpty Text -> [HS.Decl ()] -> HS.Module ()
apiModule api fns decls = HGen.mkWholeModule "Protocol.Webdriver.ClientAPI" Nothing
  (toList $ T.unpack <$> fns)
  ( [HS.OptionsPragma () (Just HS.GHC) "-Wno-missing-signatures"] <>
    HGen.mkPragmas [HSLE.DataKinds, HSLE.TypeOperators]
  )
  apiModuleImports
  ( api
    : createApiProxy
    <> [createClientAPIFunctions fns]
    <> decls
  )

typesModule :: Tree.Forest RoutePiece -> HS.Module ()
typesModule inp = HGen.mkWholeModule "Protocol.Webdriver.ClientAPI.Types" Nothing []
  (HGen.mkPragmas [ HSLE.DeriveGeneric
                  , HSLE.MultiParamTypeClasses
                  , HSLE.FlexibleInstances
                  , HSLE.FlexibleContexts
                  ]
  )
  imports
  ( wdDecl
    : vecEncInstance
    : vecDecInstance
    : jsonInstance (HGen.unqname "JsonDecode") (HS.name "mkDecoder") (HGen.moduleName "D")
    : jsonInstance (HGen.unqname "JsonEncode") (HS.name "mkEncoder") (HGen.moduleName "E")
    : createRequestTypeDecls inp
  )
  where
    wdDecl = HS.DataDecl () (HS.DataType ()) Nothing (HS.DHead () (HS.name waargTag)) [] []
    imports =
      [ HGen.importQual "GHC.Generics" "GHC"
      , textTypeImport
      , vectorTypeImport
      , HGen.importQual "Data.Vector" "V"
      , HGen.mkImportDecl "Data.Bool" [HGen.importOnly "Bool"]
      , HGen.mkImportDecl "Data.Scientific" [HGen.importOnly "Scientific"]
      , jsonTypeImport
      , HGen.importQual "Waargonaut.Encode" "E"
      , HGen.importQual "Waargonaut.Decode" "D"
      , HGen.mkImportDecl "Waargonaut.Generic"
        [ HGen.importAllOf "Tagged"
        , HGen.importOnly "Generic"
        , HGen.importOnly "HasDatatypeInfo"
        , HGen.importAllOf "JsonDecode"
        , HGen.importAllOf "JsonEncode"
        ]
      ]

jsonInstance :: HS.QName () -> HS.Name () -> HS.ModuleName () -> HS.Decl ()
jsonInstance tcname fname qmodname = HS.InstDecl () Nothing instrule (Just [decl])
  where
    jsonTy     = HS.TyCon () $ HGen.unqname "Json"
    waargtagty = HS.TyCon () $ HGen.unqname waargTag
    insthead   = HS.IHApp () (HS.IHApp () (HS.IHCon () tcname) waargtagty) jsonTy
    instrule   = HS.IRule () Nothing Nothing insthead
    decl       = HS.InsDecl () $ HS.FunBind () [match]
    match      = HS.Match () fname [] (HS.UnGuardedRhs () rhs) Nothing
    rhs        = HS.app (HS.var (HS.name "Tagged")) (HS.qvar qmodname (HS.name "json"))

mkvecInstance :: HS.Name () -> HS.QName () -> HS.Exp () -> HS.Decl ()
mkvecInstance fname tcname lam = HS.InstDecl () Nothing instrule (Just [mkdecl])
  where
    fmapsym    = HS.sym "<$>"
    tvarA      = HS.TyVar () $ HS.name "a"
    waargtagty = HS.TyCon () $ HGen.unqname waargTag
    cx         = HS.CxSingle () $ HS.ClassA () tcname [waargtagty, tvarA]

    vectorA    = HS.TyParen () $ HS.TyApp () (HS.TyCon () $ HGen.unqname "Vector") tvarA
    insthead   = HS.IHApp () (HS.IHApp () (HS.IHCon () tcname) waargtagty) vectorA
    instrule   = HS.IRule () Nothing (Just cx) insthead

    mkdecl     = HS.InsDecl () $ HS.FunBind () [mkE]
    mkE        = HS.Match () fname [] (HS.UnGuardedRhs () rhs) Nothing

    rhs        = HS.infixApp (HS.paren lam) (HS.op fmapsym) (HS.var fname)

vecEncInstance :: HS.Decl ()
vecEncInstance = mkvecInstance (HS.name "mkEncoder") (HGen.unqname "JsonEncode") lam
  where
    varE = HS.name "e"
    trav = HS.qvar (HGen.moduleName "E") (HS.name "traversable")
    lam  = HS.lamE [HS.pvar varE] $ HS.app trav (HS.var varE)

vecDecInstance :: HS.Decl ()
vecDecInstance = mkvecInstance (HS.name "mkDecoder") (HGen.unqname "JsonDecode") lam
  where
    qualFn = HS.qvar (HGen.moduleName "D")
    vempty = HS.qvar (HGen.moduleName "V") (HS.name "empty")
    withC  = qualFn (HS.name "withCursor")
    rwsnoc = qualFn (HS.name "rightwardSnoc")
    varD   = HS.name "d"
    lam    = HS.lamE [HS.pvar varD] . HS.app withC . HS.paren $ HS.app (HS.app rwsnoc vempty) (HS.var varD)
