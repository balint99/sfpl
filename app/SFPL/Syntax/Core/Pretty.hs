{-# LANGUAGE LambdaCase #-}

-- | Printing elements of the core syntax.
module SFPL.Syntax.Core.Pretty where

import Prelude hiding ((<>))

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Array.IArray hiding (Ix)
import Data.Char (showLitChar)
import Data.List (foldl')
import SFPL.Base
import SFPL.Syntax.Core.Types
import qualified SFPL.Syntax.Raw.Pretty as Raw (prettyCtrPat)
import SFPL.Utils
import Text.PrettyPrint

----------------------------------------
-- Printing types

-- | Information context for printing types: The names of bound type variables,
-- metavariables and defined types.
--
-- @since 1.0.0
type TyPCxt = ([TyName], Array Metavar TyName, Array Lvl TyName)

-- | Create an information context for types from the given list of
-- type variable names, metavariable-name associations and list of type names.
--
-- @since 1.0.0
tyPCxt :: [TyName] -> [(Metavar, TyName)] -> [TyName] -> TyPCxt
tyPCxt xs ms ts = (xs, assocArr ms, arr ts)

tyBind :: TyName -> TyPCxt -> TyPCxt
tyBind x (xs, ms, ts) = (xs :> x, ms, ts)

prettyData :: Bool -> Prec -> TyPCxt -> TyName -> TSpine -> Doc
prettyData o p cxt x sp
  | x == dsList = case sp of
    [a] -> brackets $ prettyTy o LowP cxt a
    _   -> devError "list type doesn't have 1 type parameter"
  | otherwise   = case sp of
    []      -> text x
    sp :> a -> par p AppP $ prettyData o AppP cxt x sp <+> prettyTy o AtomP cxt a

prettyMetavar :: Metavar -> Doc
prettyMetavar (Metavar m) = char '?' <> int m

prettyMeta :: Prec -> TyPCxt -> Metavar -> TSpine -> Doc
prettyMeta p cxt m = \case
  []      -> prettyMetavar m
  sp :> a -> par p AppP $ prettyMeta AppP cxt m sp <+> prettyTy False AtomP cxt a

prettyFreshMeta :: Prec -> [TyName] -> Metavar -> Doc
prettyFreshMeta p xs m = case xs of
  []      -> prettyMetavar m
  xs :> x -> par p AppP $ prettyFreshMeta AppP xs m <+> text x

prettyForAll :: Bool -> TyPCxt -> Ty -> Doc
prettyForAll o cxt = \case
  ForAll x a  -> space <> text x <> prettyForAll o (tyBind x cxt) a
  a           -> char '.' <+> prettyTy o LowP cxt a

-- | Pretty-print a type in the given precedence context, using the given
-- information context. The first parameter tells whether metavariables should be
-- printed by name or as a top-level function applied to its spine.
--
-- @since 1.0.0
prettyTy :: Bool -> Prec -> TyPCxt -> Ty -> Doc
prettyTy o p cxt@(xs, ms, ts) = \case
  TyVar i     -> text $ xs !! unIx i
  Data l sp   -> prettyData o p cxt (ts ! l) sp
  Meta m sp   -> if o then text $ ms ! m else prettyMeta p cxt m sp
  FreshMeta m -> if o then text $ ms ! m else prettyFreshMeta p xs m
  Int         -> text kwInt
  Float       -> text kwFloat
  Char        -> text kwChar
  Tuple as    -> parens . hjoin ", " $ map (prettyTy o LowP cxt) as
  World a     -> par p P9 $ char '%' <+> prettyTy o AppP cxt a
  Fun a b     -> par p P0 $ prettyTy o P1 cxt a <+> text "->" <+> prettyTy o LowP cxt b
  ForAll x a  -> par p LowP $ char '@' <+> text x <> prettyForAll o (tyBind x cxt) a

-- | Convert a type to a pretty string, using the given information context.
--
-- @since 1.0.0
showTyPrec :: Bool -> Prec -> TyPCxt -> Ty -> String
showTyPrec o p cxt a = render $ prettyTy o p cxt a

-- | Same as 'showTyPrec', with the lowest precedence.
--
-- @since 1.0.0
showTy :: Bool -> TyPCxt -> Ty -> String
showTy o = showTyPrec o LowP

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
prettyCtr p x args
  | x == dsNil  = case args of
    []  -> brackets empty
    _   -> devError "nil constructor doesn't have 0 arguments"
  | x == dsCons = case args of
    [Left x, Left y]  -> text x <+> text "::" <+> text y
    _                 -> devError "cons constructor doesn't have 2 explicit arguments"
  | otherwise   = Raw.prettyCtrPat p x args

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

----------------------------------------
-- Printing terms

-- | Information context for printing terms: the names of bound variables,
-- top-level definitions, bound type variables, metavariables,
-- defined types and data constructors.
--
-- @since 1.0.0
type TmPCxt = ([Name], Array Lvl Name, TyPCxt, PatPCxt)

-- | Create an information context for terms from the given information:
-- names of bound variables, top-level definitions, bound type variables,
-- metavariables, defined types and data constructors.
--
-- @since 1.0.0
tmPCxt :: [Name] -> [Name] ->
  [TyName] -> [(Metavar, TyName)] -> [TyName] -> [Name] -> TmPCxt
tmPCxt xs tls ys ms ts cs = (xs, arr tls, tyPCxt ys ms ts, patPCxt cs)

tmBind :: Name -> TmPCxt -> TmPCxt
tmBind x (xs, tls, tcxt, pcxt) = (xs :> x, tls, tcxt, pcxt)

tmBindTy :: TyName -> TmPCxt -> TmPCxt
tmBindTy y (xs, tls, tcxt, pcxt) = (xs, tls, tyBind y tcxt, pcxt)

prettyTySig :: Bool -> TyPCxt -> Name -> Ty -> Doc
prettyTySig o tcxt x a = text x <+> char ':' <+> prettyTy o LowP tcxt a

prettyExplBind :: Bool -> TyPCxt -> Name -> Ty -> Doc
prettyExplBind o tcxt x a = parens $ prettyTySig o tcxt x a

prettyLam :: Bool -> TmPCxt -> Tm -> Doc
prettyLam o cxt@(_, _, tcxt, _) = \case
  Lam x a t -> prettyExplBind o tcxt x a <> prettyLam o (tmBind x cxt) t
  LamI x t  -> text " {" <> text x <> prettyLamI o (tmBindTy x cxt) t
  t         -> char '.' <+> prettyTm o LowP cxt t

prettyLamI :: Bool -> TmPCxt -> Tm -> Doc
prettyLamI o cxt@(_, _, tcxt, _) = \case
  Lam x a t -> text "} " <> prettyExplBind o tcxt x a <> prettyLam o (tmBind x cxt) t
  LamI x t  -> space <> text x <> prettyLamI o (tmBindTy x cxt) t
  t         -> text "}." <+> prettyTm o LowP cxt t

-- Get the reversed spine of an implicit application, as well as the principal term.
appISpine :: Tm -> (Tm, [Ty])
appISpine = \case
  AppI t a  -> second (a :) $ appISpine t
  t         -> (t, [])

prettyAppI :: Bool -> TmPCxt -> Tm -> Ty -> Doc
prettyAppI o cxt@(_, _, tcxt, _) t a =
  let (t', as) = appISpine t
      spine = map (prettyTy o LowP tcxt) $ reverse (a : as)
  in prettyTm o AppP cxt t' <+> braces (hjoin ", " spine)

-- | Local binding in a let or bind expression.
type LocalBind = (Name, Ty, Tm)

-- Get the local bindings after a let expression.
letBinds :: Tm -> ([LocalBind], Tm)
letBinds = \case
  Let x a t u -> first ((x, a, t) : ) $ letBinds u
  t           -> ([], t)

prettyLetBind :: Bool -> LocalBind -> State TmPCxt Doc
prettyLetBind o (x, a, t) = do
  cxt@(_, _, tcxt, _) <- get
  put $ tmBind x cxt
  pure $ prettyTySig o tcxt x a
      $$ nest 2 (char '=' <+> prettyTm o LowP cxt t)

prettyLet :: Bool -> TmPCxt -> Name -> Ty -> Tm -> Tm -> Doc
prettyLet o cxt x a t u = let (bs, u') = letBinds u
                              mbs = mapM (prettyLetBind o) $ (x, a, t) : bs
                              (defs, cxt') = runState mbs cxt
                          in  text "let"
                           $$ nest 4 (vjoin ";" defs)
                           $$ text "in" <+> prettyTm o LowP cxt' u'

prettyIntLit :: Integer -> Doc
prettyIntLit = integer

prettyFloatLit :: Double -> Doc
prettyFloatLit = double

prettyCharLit :: Char -> Doc
prettyCharLit c = quotes . text $ showLitChar c ""

prettyKwApp :: Bool -> Prec -> TmPCxt -> Keyword -> [Tm] -> Doc
prettyKwApp o p cxt kw ts = par p AppP $ text kw <+> hsep (map (prettyTm o AtomP cxt) ts)

prettyUnOp :: Bool -> Prec -> TmPCxt -> UnaryOp -> Tm -> Doc
prettyUnOp o p cxt op t =
  let (opP, opSymbol) = unOpDetails op
  in par p opP $ text opSymbol <+> prettyTm o (succ opP) cxt t

prettyBinOp :: Bool -> Prec -> TmPCxt -> BinaryOp -> Tm -> Tm -> Doc
prettyBinOp o p cxt op t u =
  let (opP, assoc, opSymbol) = binOpDetails op
      (leftP, rightP) = case assoc of
                          None        -> (opP     , opP     )
                          LeftAssoc   -> (opP     , succ opP)
                          RightAssoc  -> (succ opP, opP     )
  in par p opP $ prettyTm o leftP cxt t <+> text opSymbol <+> prettyTm o rightP cxt u

prettyCaseBranch :: Bool -> TmPCxt -> CaseBranch -> Doc
prettyCaseBranch o cxt@(_, _, _, pcxt) (pat, t) =
  prettyPat LowP pcxt pat <> char '.' <+> prettyTm o LowP cxt t

prettyCase :: Bool -> TmPCxt -> Tm -> [CaseBranch] -> Doc
prettyCase o cxt t = \case
  []  -> text "case" <+> prettyTm o LowP cxt t <+> text "of {}"
  bs  -> text "case" <+> prettyTm o LowP cxt t <+> text "of {"
      $$ nest 2 (vjoin ";" $ map (prettyCaseBranch o cxt) bs)
      $$ char '}'

-- Get the local bindings after a bind expression.
bindBinds :: Tm -> ([LocalBind], Tm)
bindBinds = \case
  Bind x a t u  -> first ((x, a, t) : ) $ bindBinds u
  t             -> ([], t)

prettyBindBind :: Bool -> LocalBind -> State TmPCxt Doc
prettyBindBind o (x, a, t) = do
  cxt@(_, _, tcxt, _) <- get
  put $ tmBind x cxt
  pure $ prettyTySig o tcxt x a
      $$ nest 2 (text "<-" <+> prettyTm o LowP cxt t)

prettyBind :: Bool -> TmPCxt -> Name -> Ty -> Tm -> Tm -> Doc
prettyBind o cxt x a t u = let (bs, u') = bindBinds u
                               mbs = mapM (prettyBindBind o) $ (x, a, t) : bs
                               (binds, cxt') = runState mbs cxt
                           in  text "do"
                            $$ nest 3 (vjoin ";" binds)
                            $$ text "then" <+> prettyTm o LowP cxt' u'

-- | Pretty-print a term in the given precedence context, using the given
-- information context. The first parameter tells whether metavariables should be
-- printed by name or as a top-level function applied to its spine.
--
-- @since 1.0.0
prettyTm :: Bool -> Prec -> TmPCxt -> Tm -> Doc
prettyTm o p cxt@(xs, tls, tcxt, pcxt) = \case
  Var i         -> text $ xs !! unIx i
  TopLevel l    -> text $ tls ! l
  Lam x a t     -> par p LowP $ char '\\' <> prettyExplBind o tcxt x a <> prettyLam o (tmBind x cxt) t
  LamI x t      -> par p LowP $ text "\\{" <> text x <> prettyLamI o (tmBindTy x cxt) t
  App t u       -> par p AppP $ prettyTm o AppP cxt t <+> prettyTm o AtomP cxt u
  AppI t a      -> par p AppP $ prettyAppI o cxt t a
  Let x a t u   -> par p LowP $ prettyLet o cxt x a t u
  IntLit n      -> prettyIntLit n
  FloatLit n    -> prettyFloatLit n
  CharLit c     -> prettyCharLit c
  Tup ts        -> parens . hjoin ", " $ map (prettyTm o LowP cxt) ts
  Ctr l         -> text $ pcxt ! l
  UnOp op t     -> prettyUnOp o p cxt op t
  BinOp op t u  -> prettyBinOp o p cxt op t u
  NullFunc f    -> text (nullFuncName f)
  UnFunc f t    -> prettyKwApp o p cxt (unFuncName f) [t]
  BinFunc f t u -> prettyKwApp o p cxt (binFuncName f) [t, u]
  Case t bs     -> par p BlockP $ prettyCase o cxt t bs
  Bind x a t u  -> par p LowP $ prettyBind o cxt x a t u
  Hole          -> char '_'

-- | Convert a term to a pretty string, using the given information context.
--
-- @since 1.0.0
showTmPrec :: Bool -> Prec -> TmPCxt -> Tm -> String
showTmPrec o p cxt t = render $ prettyTm o p cxt t

-- | Same as 'showTmPrec', with the lowest precedence.
--
-- @since 1.0.0
showTm :: Bool -> TmPCxt -> Tm -> String
showTm o = showTmPrec o LowP

-- | Information context for printing top-level definitions:
-- the names of all top-level definitions, metavariables, defined types and
-- data constructors.
--
-- @since 1.0.0
type TLPCxt = (Array Lvl Name, Array Metavar TyName, Array Lvl TyName, Array Lvl Name)

-- | Create an information context for top-level definitions from the given information:
-- names of top-level definitions, metavariables, defined types and data constructors.
--
-- @since 1.0.0
tlPCxt :: [Name] -> [(Metavar, TyName)] -> [TyName] -> [Name] -> TLPCxt
tlPCxt tls ms ts cs = (arr tls, assocArr ms, arr ts, arr cs)

-- | Pretty-print a top-level definition. The first parameter tells
-- whether metavariables should be printed by name or as a
-- top-level function applied to its spine.
--
-- @since 1.0.0
prettyTopLevelDef :: Bool -> Prec -> TLPCxt -> TopLevelDef -> Doc
prettyTopLevelDef o p cxt@(tls, ms, ts, cs) (TL x a t) =
  par p LowP $ prettyTySig o tyCxt x a
            $$ nest 2 (char '=' <+> prettyTm o LowP tmCxt t) <> char ';'
  where
    tyCxt = ([], ms, ts)
    tmCxt = ([], tls, tyCxt, cs)

-- | Convert a top-level definition to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showTopLevelDefPrec :: Bool -> Prec -> TLPCxt -> TopLevelDef -> String
showTopLevelDefPrec o p cxt tl = render $ prettyTopLevelDef o p cxt tl

-- | Same as 'showTopLevelDefPrec', with the lowest precedence.
--
-- @since 1.0.0
showTopLevelDef :: Bool -> TLPCxt -> TopLevelDef -> String
showTopLevelDef o = showTopLevelDefPrec o LowP

----------------------------------------
-- Printing type declarations

-- | Information context for printing data constructor declarations:
-- the names of bound type variables, defined types and data constructors.
--
-- @since 1.0.0
type CtrPCxt = ([TyName], Array Lvl TyName, Array Lvl Name)

-- | Create an information context for data constructor declarations
-- from the given information: the type variables bound by the owning
-- data type, the names of defined types and data constructors.
--
-- @since 1.0.0
ctrPCxt :: [TyName] -> [TyName] -> [Name] -> CtrPCxt
ctrPCxt xs ts cs = (reverse xs, arr ts, arr cs)

-- | Pretty-print a data constructor declaration.
--
-- @since 1.0.0
prettyConstructor :: Prec -> CtrPCxt -> Constructor -> Doc
prettyConstructor p cxt@(xs, ts, cs) (Constructor l a) =
  par p LowP $ prettyTySig False tcxt (cs ! l) a
  where
    tcxt = (xs, assocArr [], ts)

-- | Information context for printing data type declarations:
-- the names of defined types and data constructors.
--
-- @since 1.0.0
type DDPCxt = (Array Lvl TyName, Array Lvl Name)

-- | Create an information context for data type declarations
-- from the given information: the names of defined types and data constructors.
--
-- @since 1.0.0
ddPCxt :: [TyName] -> [Name] -> DDPCxt
ddPCxt ts cs = (arr ts, arr cs)

prettyConstructors :: CtrPCxt -> Constructor -> [Constructor] -> Doc
prettyConstructors ccxt c cs =
    char '=' <+> prettyConstructor LowP ccxt c
 $$ vcat (map (\c -> char '|' <+> prettyConstructor LowP ccxt c) cs)

-- | Pretty-print a data type declaration.
--
-- @since 1.0.0
prettyDataDecl :: Prec -> DDPCxt -> DataDecl -> Doc
prettyDataDecl p cxt@(ts, ctrs) (DD l xs cs) = par p LowP $ case cs of
  []      -> text "data" <+> text (ts ! l) <+> hsep (map text xs) <> char ';'
  c : cs  -> text "data" <+> text (ts ! l) <+> hsep (map text xs)
          $$ nest 2 (prettyConstructors ccxt c cs <> char ';')
  where
    ccxt = (reverse xs, ts, ctrs)

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

------------------------------------------------------------
-- Printing programs

-- | Information context for printing programs:
-- the names of all top-level definitions, metavariables, defined types and
-- data constructors.
--
-- @since 1.0.0
type ProgPCxt = TLPCxt

-- | Create an information context for programs from the given information:
-- names of top-level definitions, metavariables, defined types and data constructors.
--
-- @since 1.0.0
progPCxt :: [Name] -> [(Metavar, TyName)] -> [TyName] -> [Name] -> ProgPCxt
progPCxt = tlPCxt

-- | Pretty-print a program. The first parameter tells
-- whether metavariables should be printed by name or as a
-- top-level function applied to its spine.
--
-- @since 1.0.0
prettyProgram :: Bool -> Prec -> ProgPCxt -> Program -> Doc
prettyProgram o p cxt@(tls, ms, ts, cs) =
  par p LowP . vcat
  . map (endDef . either (prettyTypeDecl p tdcxt) (prettyTopLevelDef o p tlcxt))
  where
    endDef doc = doc $$ text ""
    tdcxt = (ts, cs)
    tlcxt = cxt

-- | Convert a program to a pretty string,
-- using the given information context.
--
-- @since 1.0.0
showProgramPrec :: Bool -> Prec -> ProgPCxt -> Program -> String
showProgramPrec o p cxt pr = render $ prettyProgram o p cxt pr

-- | Same as 'showProgramPrec', with the lowest precedence.
--
-- @since 1.0.0
showProgram :: Bool -> ProgPCxt -> Program -> String
showProgram o = showProgramPrec o LowP
