{-# LANGUAGE LambdaCase #-}

-- | Printing elements of the core syntax.
module SFPL.Syntax.Core.Pretty where

import Prelude hiding ((<>))

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Array.IArray hiding (Ix)
import qualified Data.Array.IArray as Arr (Ix)
import Data.Char (showLitChar)
import Data.List (foldl')
import SFPL.Base
import SFPL.Syntax.Core.Types
import qualified SFPL.Syntax.Raw.Pretty as Raw (prettyCtrPat)
import SFPL.Utils
import Text.PrettyPrint

----------------------------------------
-- Printing types

-- | Information context for printing types: The names of bound type variables
-- and the names of defined types.
--
-- @since 1.0.0
type TyPCxt = ([TyName], Array Lvl TyName)

-- | Create an information context for types from the given list of type names.
--
-- @since 1.0.0
tyPCxt :: [TyName] -> TyPCxt
tyPCxt ts = ([], arr ts)

tyBind :: TyName -> TyPCxt -> TyPCxt
tyBind x (xs, ts) = (xs :> x, ts)

prettyData :: Prec -> TyPCxt -> TyName -> TSpine -> Doc
prettyData p cxt x = \case
  []      -> text x
  sp :> a -> par p AppP $ prettyData AppP cxt x sp <+> prettyTy AtomP cxt a

prettyMetavar :: Metavar -> Doc
prettyMetavar (Metavar m) = char '?' <> int m

prettyMeta :: Prec -> TyPCxt -> Metavar -> TSpine -> Doc
prettyMeta p cxt m = \case
  []      -> prettyMetavar m
  sp :> a -> par p AppP $ prettyMeta AppP cxt m sp <+> prettyTy AtomP cxt a

prettyFreshMeta :: Prec -> [TyName] -> Metavar -> Doc
prettyFreshMeta p xs m = case xs of
  []      -> prettyMetavar m
  xs :> x -> par p AppP $ prettyFreshMeta AppP xs m <+> text x

prettyForAll :: TyPCxt -> Ty -> Doc
prettyForAll cxt = \case
  ForAll x a  -> space <> text x <> prettyForAll (tyBind x cxt) a
  a           -> char '.' <+> prettyTy LowP cxt a

-- | Pretty-print a type in the given precedence context, using the given
-- information context.
--
-- @since 1.0.0
prettyTy :: Prec -> TyPCxt -> Ty -> Doc
prettyTy p cxt@(xs, ts) = \case
  TyVar i     -> text $ xs !! unIx i
  Data l sp   -> prettyData p cxt (ts ! l) sp
  Meta m sp   -> prettyMeta p cxt m sp
  FreshMeta m -> prettyFreshMeta p xs m
  Int         -> text kwInt
  Float       -> text kwFloat
  Char        -> text kwChar
  Tuple as    -> parens . hjoin ", " $ map (prettyTy LowP cxt) as
  World a     -> par p P9 $ char '%' <+> prettyTy AppP cxt a
  Fun a b     -> par p P0 $ prettyTy P1 cxt a <+> text "->" <+> prettyTy LowP cxt b
  ForAll x a  -> par p LowP $ char '@' <+> text x <> prettyForAll (tyBind x cxt) a

-- | Convert a type to a pretty string, using the given information context.
--
-- @since 1.0.0
showTyPrec :: Prec -> TyPCxt -> Ty -> String
showTyPrec p cxt a = render $ prettyTy p cxt a

-- | Same as 'showTyPrec LowP'.
--
-- @since 1.0.0
showTy :: TyPCxt -> Ty -> String
showTy = showTyPrec LowP

----------------------------------------
-- Printing patterns

-- | Information context for printing patterns: the names of the constructors of
-- defined data types.
--
-- @since 1.0.0
type PatPCxt = Array Lvl Name

-- | Create an information context for patterns from the given list of constructor names.
-- An inner list corresponds to the constructors of one data type.
--
-- @since 1.0.0
patPCxt :: [Name] -> PatPCxt
patPCxt = arr

prettyCtr :: Prec -> Name -> CtrArgs -> Doc
prettyCtr = Raw.prettyCtrPat

-- | Pretty-print a pattern in the given precedence context, using the given
-- information context.
--
-- @since 1.0.0
prettyPat :: Prec -> PatPCxt -> Pattern -> Doc
prettyPat p cxt = \case
  PInt n    -> prettyIntLit n
  PFloat n  -> prettyFloatLit n
  PChar c   -> prettyCharLit c
  PTuple xs -> parens . hjoin ", " $ map text xs
  PCtr l bs -> prettyCtr p (cxt ! l) bs
  PWildcard -> text "_"

-- | Convert a pattern to a pretty string, using the given information context.
--
-- @since 1.0.0
showPatPrec :: Prec -> PatPCxt -> Pattern -> String
showPatPrec p cxt pat = render $ prettyPat p cxt pat

-- | Same as 'showPatPrec LowP'.
--
-- @since 1.0.0
showPat :: PatPCxt -> Pattern -> String
showPat = showPatPrec LowP

----------------------------------------
-- Printing terms

-- | Information context for printing terms: the names of bound variables,
-- top-level definitions, bound type variables, defined types and
-- data constructors.
--
-- @since 1.0.0
type TmPCxt = ([Name], Array Lvl Name, TyPCxt, PatPCxt)

-- | Create an information context for terms from the given information:
-- names of top-level definitions, defined types and data constructors.
--
-- @since 1.0.0
tmPCxt :: [Name] -> [TyName] -> [Name] -> TmPCxt
tmPCxt tls ts cs = ([], arr tls, tyPCxt ts, patPCxt cs)

tmBind :: Name -> TmPCxt -> TmPCxt
tmBind x (xs, tls, tcxt, pcxt) = (xs :> x, tls, tcxt, pcxt)

tmBindTy :: TyName -> TmPCxt -> TmPCxt
tmBindTy y (xs, tls, tcxt, pcxt) = (xs, tls, tyBind y tcxt, pcxt)

prettyTySig :: TyPCxt -> Name -> Ty -> Doc
prettyTySig tcxt x a = text x <+> char ':' <+> prettyTy LowP tcxt a

prettyExplBind :: TyPCxt -> Name -> Ty -> Doc
prettyExplBind tcxt x a = parens $ prettyTySig tcxt x a

prettyLam :: TmPCxt -> Tm -> Doc
prettyLam cxt@(_, _, tcxt, _) = \case
  Lam x a t -> prettyExplBind tcxt x a <> prettyLam (tmBind x cxt) t
  LamI x t  -> text " {" <> text x <> prettyLamI (tmBindTy x cxt) t
  t         -> char '.' <+> prettyTm LowP cxt t

prettyLamI :: TmPCxt -> Tm -> Doc
prettyLamI cxt@(_, _, tcxt, _) = \case
  Lam x a t -> text "} " <> prettyExplBind tcxt x a <> prettyLam (tmBind x cxt) t
  LamI x t  -> space <> text x <> prettyLamI (tmBindTy x cxt) t
  t         -> text "}." <+> prettyTm LowP cxt t

-- Get the reversed spine of an implicit application, as well as the principal term.
appISpine :: Tm -> (Tm, [Ty])
appISpine = \case
  AppI t a  -> second (a :) $ appISpine t
  t         -> (t, [])

prettyAppI :: TmPCxt -> Tm -> Ty -> Doc
prettyAppI cxt@(_, _, tcxt, _) t a =
  let (t', as) = appISpine t
      spine = map (prettyTy LowP tcxt) $ reverse (a : as)
  in prettyTm AppP cxt t' <+> braces (hjoin ", " spine)

-- | Local binding in a let or bind expression.
type LocalBind = (Name, Ty, Tm)

-- Get the local bindings after a let expression.
letBinds :: Tm -> ([LocalBind], Tm)
letBinds = \case
  Let x a t u -> first ((x, a, t) : ) $ letBinds u
  t           -> ([], t)

prettyLetBind :: LocalBind -> State TmPCxt Doc
prettyLetBind (x, a, t) = do
  cxt@(_, _, tcxt, _) <- get
  put $ tmBind x cxt
  pure $ prettyTySig tcxt x a
      $$ nest 2 (char '=' <+> prettyTm LowP cxt t)

prettyLet :: TmPCxt -> Name -> Ty -> Tm -> Tm -> Doc
prettyLet cxt x a t u = let (bs, u') = letBinds u
                            mbs = mapM prettyLetBind $ (x, a, t) : bs
                            (defs, cxt') = runState mbs cxt
                        in  text "let"
                         $$ nest 4 (vjoin ";" defs)
                         $$ text "in" <+> prettyTm LowP cxt' u'

prettyIntLit :: Integer -> Doc
prettyIntLit = integer

prettyFloatLit :: Double -> Doc
prettyFloatLit = double

prettyCharLit :: Char -> Doc
prettyCharLit c = quotes . text $ showLitChar c ""

prettyKwApp :: Prec -> TmPCxt -> Keyword -> [Tm] -> Doc
prettyKwApp p cxt kw ts = par p AppP $ text kw <+> hsep (map (prettyTm AtomP cxt) ts)

prettyUnOp :: Prec -> TmPCxt -> UnaryOp -> Tm -> Doc
prettyUnOp p cxt op t =
  let (opP, opSymbol) = unOpDetails op
  in par p opP $ text opSymbol <+> prettyTm (succ opP) cxt t

prettyBinOp :: Prec -> TmPCxt -> BinaryOp -> Tm -> Tm -> Doc
prettyBinOp p cxt op t u =
  let (opP, assoc, opSymbol) = binOpDetails op
      (leftP, rightP) = case assoc of
                          None        -> (opP     , opP     )
                          LeftAssoc   -> (opP     , succ opP)
                          RightAssoc  -> (succ opP, opP     )
  in par p opP $ prettyTm leftP cxt t <+> text opSymbol <+> prettyTm rightP cxt u

prettyCaseBranch :: TmPCxt -> CaseBranch -> Doc
prettyCaseBranch cxt@(_, _, _, pcxt) (pat, t) =
  prettyPat LowP pcxt pat <> char '.' <+> prettyTm LowP cxt t

prettyCase :: TmPCxt -> Tm -> [CaseBranch] -> Doc
prettyCase cxt t = \case
  []  -> text "case" <+> prettyTm LowP cxt t <+> text "of {}"
  bs  -> text "case" <+> prettyTm LowP cxt t <+> text "of {"
      $$ nest 2 (vjoin ";" $ map (prettyCaseBranch cxt) bs)
      $$ char '}'

-- Get the local bindings after a bind expression.
bindBinds :: Tm -> ([LocalBind], Tm)
bindBinds = \case
  Bind x a t u  -> first ((x, a, t) : ) $ bindBinds u
  t             -> ([], t)

prettyBindBind :: LocalBind -> State TmPCxt Doc
prettyBindBind (x, a, t) = do
  cxt@(_, _, tcxt, _) <- get
  put $ tmBind x cxt
  pure $ prettyTySig tcxt x a
      $$ nest 2 (text "<-" <+> prettyTm LowP cxt t)

prettyBind :: TmPCxt -> Name -> Ty -> Tm -> Tm -> Doc
prettyBind cxt x a t u = let (bs, u') = bindBinds u
                             mbs = mapM prettyBindBind $ (x, a, t) : bs
                             (binds, cxt') = runState mbs cxt
                         in  text "do"
                          $$ nest 3 (vjoin ";" binds)
                          $$ text "then" <+> prettyTm LowP cxt' u'

-- | Pretty-print a term in the given precedence context, using the given
-- information context.
--
-- @since 1.0.0
prettyTm :: Prec -> TmPCxt -> Tm -> Doc
prettyTm p cxt@(xs, tls, tcxt, pcxt) = \case
  Var i         -> text $ xs !! unIx i
  TopLevel l    -> text $ tls ! l
  Lam x a t     -> par p LowP $ char '\\' <> prettyExplBind tcxt x a <> prettyLam (tmBind x cxt) t
  LamI x t      -> par p LowP $ text "\\{" <> text x <> prettyLamI (tmBindTy x cxt) t
  App t u       -> par p AppP $ prettyTm AppP cxt t <+> prettyTm AtomP cxt u
  AppI t a      -> par p AppP $ prettyAppI cxt t a
  Let x a t u   -> par p LowP $ prettyLet cxt x a t u
  IntLit n      -> prettyIntLit n
  FloatLit n    -> prettyFloatLit n
  CharLit c     -> prettyCharLit c
  Tup ts        -> parens . hjoin ", " $ map (prettyTm LowP cxt) ts
  Ctr l         -> text $ pcxt ! l
  UnOp op t     -> prettyUnOp p cxt op t
  BinOp op t u  -> prettyBinOp p cxt op t u
  NullFunc f    -> text (nullFuncName f)
  UnFunc f t    -> prettyKwApp p cxt (unFuncName f) [t]
  BinFunc f t u -> prettyKwApp p cxt (binFuncName f) [t, u]
  Case t bs     -> par p BlockP $ prettyCase cxt t bs
  Bind x a t u  -> par p LowP $ prettyBind cxt x a t u

-- | Convert a term to a pretty string, using the given information context.
--
-- @since 1.0.0
showTmPrec :: Prec -> TmPCxt -> Tm -> String
showTmPrec p cxt t = render $ prettyTm p cxt t

-- | Same as 'showTmPrec LowP'.
--
-- @since 1.0.0
showTm :: TmPCxt -> Tm -> String
showTm = showTmPrec LowP

-- | Information context for printing top-level definitions:
-- the names of all top-level definitions, defined types and
-- data constructors.
--
-- @since 1.0.0
type TLPCxt = (Array Lvl Name, Array Lvl TyName, Array Lvl Name)

-- | Create an information context for top-level definitions from the given information:
-- names of top-level definitions, defined types and data constructors.
--
-- @since 1.0.0
tlPCxt :: [Name] -> [TyName] -> [Name] -> TLPCxt
tlPCxt tls ts cs = (arr tls, arr ts, arr cs)

-- | Pretty-print a top-level definition.
--
-- @since 1.0.0
prettyTopLevelDef :: Prec -> TLPCxt -> TopLevelDef -> Doc
prettyTopLevelDef p cxt@(tls, ts, cs) (TL x a t) =
  par p LowP $ prettyTySig tyCxt x a
            $$ nest 2 (char '=' <+> prettyTm LowP tmCxt t) <> char ';'
  where
    tyCxt = ([], ts)
    tmCxt = ([], tls, tyCxt, cs)

-- | Convert a top-level definition to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showTopLevelDefPrec :: Prec -> TLPCxt -> TopLevelDef -> String
showTopLevelDefPrec p cxt tl = render $ prettyTopLevelDef p cxt tl

-- | Same as 'showTopLevelDefPrec LowP'.
--
-- @since 1.0.0
showTopLevelDef :: TLPCxt -> TopLevelDef -> String
showTopLevelDef = showTopLevelDefPrec LowP

----------------------------------------
-- Printing type declarations

-- | Information context for printing data constructor declarations:
-- the names of bound type variables, defined types and data constructors.
--
-- @since 1.0.0
type CtrPCxt = (TyPCxt, Array Lvl Name)

-- | Create an information context for data constructor declarations
-- from the given information: the type variables bound by the owning
-- data type, the names of defined types and data constructors.
--
-- @since 1.0.0
ctrPCxt :: [TyName] -> [TyName] -> [Name] -> CtrPCxt
ctrPCxt xs ts cs = (tcxt, arr cs)
  where
    tcxt = (reverse xs, arr ts)

-- | Pretty-print a data constructor declaration.
--
-- @since 1.0.0
prettyConstructor :: Prec -> CtrPCxt -> Constructor -> Doc
prettyConstructor p cxt@(tcxt, cs) (Constructor l a) =
  par p LowP $ prettyTySig tcxt (cs ! l) a

-- | Convert a data constructor declaration to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showConstructorPrec :: Prec -> CtrPCxt -> Constructor -> String
showConstructorPrec p cxt c = render $ prettyConstructor p cxt c

-- | Same as 'showConstructorPrec LowP'.
--
-- @since 1.0.0
showConstructor :: CtrPCxt -> Constructor -> String
showConstructor = showConstructorPrec LowP

-- | Information context for printing data type declarations:
-- the names of defined types and data constructors.
--
-- @since 1.0.0
type DDPCxt = (Array Lvl TyName, Array Lvl Name)

-- | Create an information context for data type declarations
-- from the given information: names of defined types and data constructors.
--
-- @since 1.0.0
ddPCxt :: [TyName] -> [Name] -> DDPCxt
ddPCxt ts cs = (arr ts, arr cs)

prettyConstructors :: CtrPCxt -> [Constructor] -> Doc
prettyConstructors ccxt = \case
  []      -> empty
  c : cs  -> space <> (char '=' <+> prettyConstructor LowP ccxt c
                    $$ vcat (map (\c -> char '|' <+> prettyConstructor LowP ccxt c) cs))

-- | Pretty-print a data type declaration.
--
-- @since 1.0.0
prettyDataDecl :: Prec -> DDPCxt -> DataDecl -> Doc
prettyDataDecl p cxt@(ts, cs) (DD l xs cs') =
  par p LowP $ text "data" <+> text (ts ! l) <+> hsep (map text xs)
            <> prettyConstructors ccxt cs' <> char ';'
  where
    ccxt = ((reverse xs, ts), cs)

-- | Convert a data type declaration to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showDataDeclPrec :: Prec -> DDPCxt -> DataDecl -> String
showDataDeclPrec p cxt dd = render $ prettyDataDecl p cxt dd

-- | Same as 'showDataDeclPrec LowP'.
--
-- @since 1.0.0
showDataDecl :: DDPCxt -> DataDecl -> String
showDataDecl = showDataDeclPrec LowP

-- | Information context for printing type declarations:
-- the names of defined types and data constructors.
--
-- @since 1.0.0
type TDPCxt = DDPCxt

-- | Create an information context for type declarations
-- from the given information: names of defined types and data constructors.
--
-- @since 1.0.0
tdPCxt :: [TyName] -> [Name] -> TDPCxt
tdPCxt = ddPCxt

-- | Pretty-print a type declaration.
--
-- @since 1.0.0
prettyTypeDecl :: Prec -> TDPCxt -> TypeDecl -> Doc
prettyTypeDecl p cxt = \case
  DataDecl dd -> prettyDataDecl p cxt dd

-- | Convert a type declaration to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showTypeDeclPrec :: Prec -> TDPCxt -> TypeDecl -> String
showTypeDeclPrec p cxt td = render $ prettyTypeDecl p cxt td

-- | Same as 'showTypeDeclPrec LowP'.
--
-- @since 1.0.0
showTypeDecl :: TDPCxt -> TypeDecl -> String
showTypeDecl = showTypeDeclPrec LowP
