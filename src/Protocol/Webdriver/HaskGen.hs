{-# LANGUAGE FlexibleContexts #-}
module Protocol.Webdriver.HaskGen where

import qualified Language.Haskell.Exts                  as HS
import           Language.Haskell.TH.LanguageExtensions (Extension)

import           Protocol.Webdriver.Types

unitTy :: HS.Name ()
unitTy = HS.name "()"

unqUnitTy :: HS.QName ()
unqUnitTy = unqname "()"

unqname :: String -> HS.QName ()
unqname = HS.UnQual () . HS.name

qname :: HS.ModuleName () -> String -> HS.QName ()
qname modname = HS.Qual () modname . HS.name

simpleTyDecl :: String -> HS.Type ()
simpleTyDecl = HS.TyCon () . unqname

tyApp :: String -> Typeish -> HS.Type ()
tyApp ctor inner = HS.TyParen () ( HS.TyApp () (HS.TyCon () (unqname ctor)) $ toTypeDecl inner)

promotedString :: String -> HS.Promoted ()
promotedString str = HS.PromotedString () str mempty

wrapMaybe :: HS.Type () -> HS.Type ()
wrapMaybe = HS.TyApp () (HS.TyCon () (unqname "Maybe"))

toTypeDecl :: Typeish -> HS.Type ()
toTypeDecl (Array a)                = tyApp "Vector" a
toTypeDecl (Possibly [a])           = tyApp "Maybe" a
toTypeDecl (Possibly [a,Nully])     = toTypeDecl (Possibly [a])
toTypeDecl (Possibly [a,Undefined]) = toTypeDecl (Possibly [a])
toTypeDecl (Possibly [a,LOL])       = toTypeDecl (Possibly [a])
toTypeDecl (Possibly [a,b]) = HS.TyParen () $
  HS.TyApp () (HS.TyApp () (HS.TyCon () $ unqname "Either") (toTypeDecl a)) (toTypeDecl b)

toTypeDecl t = HS.TyCon () $ case t of
  Nully     -> unqUnitTy
  Undefined -> unqUnitTy

  Stringly  -> unqname "Text"
  Numberly  -> unqname "Scientific"
  Booleanly -> unqname "Bool"

  _         -> unqname "Json"

mkPragmas :: [Extension] -> [HS.ModulePragma ()]
mkPragmas = fmap langPragma
  where
    langPragma p = HS.LanguagePragma () [HS.Ident () (show p)]

mkExports :: [String] -> HS.ExportSpecList ()
mkExports xs = HS.ExportSpecList () $ mkExport <$> xs
  where
    mkExport e = HS.EVar () (HS.UnQual () (HS.Ident () e))

mkImportDecl :: String -> [HS.ImportSpec ()] -> HS.ImportDecl ()
mkImportDecl nm ixs = defImport (moduleName nm) (mkImportSpecList ixs)
  where
    defImport mn = HS.ImportDecl () mn False False False Nothing Nothing

mkImportSpecList :: [HS.ImportSpec ()] -> Maybe (HS.ImportSpecList ())
mkImportSpecList [] = Nothing
mkImportSpecList is = pure $ HS.ImportSpecList () False is

moduleName :: String -> HS.ModuleName ()
moduleName = HS.ModuleName ()

importAllOf :: String -> HS.ImportSpec ()
importAllOf t = HS.IThingAll () (HS.Ident () t)

importOnly :: String -> HS.ImportSpec ()
importOnly t = HS.IAbs () (HS.NoNamespace ()) (HS.Ident () t)

importEntire :: String -> HS.ImportDecl ()
importEntire m = mkImportDecl m mempty

importQual :: String -> String -> HS.ImportDecl ()
importQual m as = HS.ImportDecl () (moduleName m) True False False Nothing (Just $ moduleName as) Nothing

hasSome :: [a] -> Maybe [a]
hasSome [] = Nothing
hasSome xs = Just xs

mkModuleHead
  :: String
  -> Maybe (HS.WarningText ())
  -> [String]
  -> HS.ModuleHead ()
mkModuleHead m warning exports = HS.ModuleHead ()
  (moduleName m)
  warning
  exps
  where exps = mkExports <$> hasSome exports

mkWholeModule
  :: String
  -> Maybe (HS.WarningText ())
  -> [String]
  -> [HS.ModulePragma ()]
  -> [HS.ImportDecl ()]
  -> [HS.Decl ()]
  -> HS.Module ()
mkWholeModule m warning exports pragmas = HS.Module ()
  (pure (mkModuleHead m warning exports)) pragmas

-- deriving (Show, Eq, GHC.Generic)
-- Deriving l (Maybe (DerivStrategy ()) [InstRule l]
mkDeriving :: [HS.QName ()] -> HS.Deriving ()
mkDeriving tc = HS.Deriving () Nothing mkHead -- [HS.IRule () Nothing Nothing mkHead]
  where mkHead = HS.IRule () Nothing Nothing . HS.IHCon () <$> tc

-- instance HasDatatypeInfo X
-- instance Generic X
instanceOneTy
  :: HS.QName ()
  -> HS.Type ()
  -> HS.Decl ()
instanceOneTy inst ty = HS.InstDecl ()
  Nothing
  (HS.IRule () Nothing Nothing $ HS.IHApp () (HS.IHCon () inst) ty)
  Nothing

-- instance JsonEncode WDJson
-- instance JsonDecode WDJson
instanceTwoTy
  :: HS.QName ()
  -> HS.Type ()
  -> HS.Type ()
  -> HS.Decl ()
instanceTwoTy inst tyA tyB = HS.InstDecl ()
  Nothing                                -- No type vars
  (HS.IRule () Nothing Nothing insthead) -- multiparam typeclass
  Nothing                                -- no instance body
  where
    insthead = HS.IHApp () (HS.IHApp () (HS.IHCon () inst) tyA) tyB

mkRecordType
  :: HS.Name ()
  -> HS.DataOrNew ()
  -> [(String, HS.Type ())]
  -> HS.Deriving ()
  -> HS.Decl ()
mkRecordType nm datanew fields deriv = HS.DataDecl ()
    datanew
    Nothing
    (HS.DHead () nm)
    [HS.QualConDecl () Nothing Nothing (HS.RecDecl () nm (mkField <$> fields))]
    [deriv]
  where
    mkField (f,t) = HS.FieldDecl () [HS.Ident () ("_" <> f)] t

mkRecord
  :: HS.Name ()
  -> [(String, HS.Type ())]
  -> HS.Deriving ()
  -> HS.Decl ()
mkRecord nm =
  mkRecordType nm (HS.DataType ())

mkNewType
  :: HS.Name ()
  -> String
  -> HS.Type ()
  -> HS.Deriving ()
  -> HS.Decl ()
mkNewType ctor fieldName ty =
  mkRecordType ctor (HS.NewType ()) [(fieldName,ty)]
