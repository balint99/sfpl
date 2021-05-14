{-# LANGUAGE LambdaCase #-}

-- | Printing elements of the presyntax.
module SFPL.Syntax.Raw.Pretty where

import SFPL.Syntax.Raw.Types

import Prelude hiding ((<>))

import Data.Char (showLitChar)
import Data.Either (isRight)
import Data.Maybe (isJust)
import SFPL.Base
import SFPL.Utils
import Text.PrettyPrint

----------------------------------------
-- Printing types

-- | Pretty-print a type in the given precedence context.
--
-- @since 1.0.0
prettyTy :: Prec -> Ty -> Doc
prettyTy p = \case
  TyIden x _    -> text x
  THole _       -> char '_'
  Int _ _       -> text kwInt
  Float _ _     -> text kwFloat
  Char _ _      -> text kwChar
  Tuple as _ _  -> parens . hjoin ", " $ map (prettyTy LowP) as
  List a _ _    -> brackets $ prettyTy LowP a
  World a _     -> par p P9 $ char '%' <+> prettyTy AppP a
  TApp a sp     -> par p AppP $ prettyTy AppP a <+> hsep (map (prettyTy AtomP) sp)
  Fun a b       -> par p P0 $ prettyTy P1 a <+> text "->" <+> prettyTy LowP b
  ForAll xs a _ -> par p LowP $ char '@' <+> hsep (map text xs)
                             <> char '.' <+> prettyTy LowP a

----------------------------------------
-- Printing patterns

prettyCtrArg :: Bool -> Either Name TyName -> Doc
prettyCtrArg b arg = case (b, arg) of
  (False, Left x )  -> space <> text x
  (True , Left x )  -> text "} " <> text x
  (False, Right x)  -> text " {" <> text x
  (True , Right x)  -> space <> text x

-- First parameter: whether the last argument was implicit or not
prettyCtrArgs :: Bool -> CtrArgs -> Doc
prettyCtrArgs b args = case (b, args) of
  (False, []    ) -> empty
  (True , []    ) -> char '}'
  (b    , a : as) -> prettyCtrArg b a <> prettyCtrArgs (isRight a) as

prettyCtrPat :: Prec -> Name -> CtrArgs -> Doc
prettyCtrPat p x = \case
  []    -> text x
  args  -> par p AppP $ text x <> prettyCtrArgs False args

-- | Pretty-print a pattern.
--
-- @since 1.0.0
prettyPat :: Prec -> Pattern -> Doc
prettyPat p = \case
  IntPat n _ _      -> prettyIntLit n
  FloatPat n _ _    -> prettyFloatLit n
  CharPat c _ _     -> prettyCharLit c
  TuplePat xs _ _   -> parens . hjoin ", " $ map text xs
  EmptyListPat _ _  -> text "[]"
  ConsPat x y _ _   -> par p LowP $ text x <+> text "::" <+> text y
  CtrPat x args _ _ -> prettyCtrPat p x args
  WildcardPat _     -> char '_'

----------------------------------------
-- Printing terms

prettyTySig :: Name -> Ty -> Doc
prettyTySig x a = text x <+> char ':' <+> prettyTy LowP a

prettyExplBind :: Name -> Maybe Ty -> Doc
prettyExplBind x = \case
  Nothing -> text x
  Just a  -> parens (prettyTySig x a)

-- First parameter: whether the last binder was an explicit binder with a type signature
prettyLamBindsExpl :: Bool -> [LamBind] -> Doc
prettyLamBindsExpl a = \case
  []      -> empty
  b : bs  -> case b of
    Expl x ma -> (if a && isJust ma then empty else space)
                  <> prettyExplBind x ma <> prettyLamBindsExpl (isJust ma) bs
    Impl x    -> text " {" <> text x <> prettyLamBindsImpl bs

prettyLamBindsImpl :: [LamBind] -> Doc
prettyLamBindsImpl = \case
  []      -> char '}'
  b : bs  -> case b of
    Expl x ma -> text "} " <> prettyExplBind x ma <> prettyLamBindsExpl (isJust ma) bs
    Impl x    -> space <> text x <> prettyLamBindsImpl bs

prettyLam :: [LamBind] -> Tm -> Doc
prettyLam (b : bs) t =
  let binders = case b of
        Expl x ma -> prettyExplBind x ma <> prettyLamBindsExpl (isJust ma) bs
        Impl x    -> char '{' <> text x <> prettyLamBindsImpl bs
  in char '\\' <> binders <> char '.' <+> prettyTm LowP t

prettyLetBind :: LocalBind -> Doc
prettyLetBind (x, ma, t) = case ma of
  Nothing -> text x <+> char '=' <+> prettyTm LowP t
  Just a  -> prettyTySig x a $$ nest 2 (char '=' <+> prettyTm LowP t)

prettyLet :: [LocalBind] -> Tm -> Doc
prettyLet bs t = text "let"
              $$ nest 4 (vjoin ";" $ map prettyLetBind bs)
              $$ text "in" <+> prettyTm LowP t

prettyIntLit :: Integer -> Doc
prettyIntLit = integer

prettyFloatLit :: Double -> Doc
prettyFloatLit = double

prettyChar :: Char -> Doc
prettyChar c = text $ showLitChar c ""

prettyCharLit :: Char -> Doc
prettyCharLit = quotes . prettyChar

prettyStringLit :: String -> Doc
prettyStringLit = doubleQuotes . hcat . map prettyChar

prettyKwApp :: Prec -> Keyword -> [Tm] -> Doc
prettyKwApp p kw ts = par p AppP $ text kw <+> hsep (map (prettyTm AtomP) ts)

prettyUnOp :: Prec -> UnaryOp -> Tm -> Doc
prettyUnOp p op t =
  let (opP, opSymbol) = unOpDetails op
  in par p opP $ text opSymbol <+> prettyTm (succ opP) t

prettyBinOp :: Prec -> BinaryOp -> Tm -> Tm -> Doc
prettyBinOp p op t u =
  let (opP, assoc, opSymbol) = binOpDetails op
      (leftP, rightP) = case assoc of
                          None        -> (opP     , opP     )
                          LeftAssoc   -> (opP     , succ opP)
                          RightAssoc  -> (succ opP, opP     )
  in par p opP $ prettyTm leftP t <+> text opSymbol <+> prettyTm rightP u

prettyIf :: Tm -> Tm -> Tm -> Doc
prettyIf t u v = text "if" <+> prettyTm LowP t
             <+> text "then" <+> prettyTm LowP u
             <+> text "else" <+> prettyTm LowP v

prettySplit :: Tm -> [Name] -> Tm -> Doc
prettySplit t xs u = text "split" <+> prettyTm LowP t
                 <+> text "as" <+> parens (hjoin ", " $ map text xs)
                  <> char '.' <+> prettyTm LowP u

prettySwitchBranch :: SwitchBranch -> Doc
prettySwitchBranch (t, u) = prettyTm LowP t <+> text "->" <+> prettyTm LowP u

prettySwitch :: [SwitchBranch] -> Doc
prettySwitch = \case
  []  -> text "switch {}"
  bs  -> text "switch {"
      $$ nest 2 (vjoin ";" $ map prettySwitchBranch bs)
      $$ char '}'

prettyCaseBranch :: CaseBranch -> Doc
prettyCaseBranch (pat, t) = prettyPat LowP pat <> char '.' <+> prettyTm LowP t

prettyCase :: Tm -> [CaseBranch] -> Doc
prettyCase t = \case
  []  -> text "case" <+> prettyTm LowP t <+> text "of {}"
  bs  -> text "case" <+> prettyTm LowP t <+> text "of {"
      $$ nest 2 (vjoin ";" $ map prettyCaseBranch bs)
      $$ char '}' 

prettyDoBind :: LocalBind -> Doc
prettyDoBind (x, ma, t) = case ma of
  Nothing -> text x <+> text "<-" <+> prettyTm LowP t
  Just a  -> prettyTySig x a $$ nest 2 (text "<-" <+> prettyTm LowP t)

prettyDo :: [LocalBind] -> Tm -> Doc
prettyDo bs t = text "do"
             $$ nest 3 (vjoin ";" $ map prettyDoBind bs)
             $$ text "then" <+> prettyTm LowP t

-- | Pretty-print a term in the given precedence context.
--
-- @since 1.0.0
prettyTm :: Prec -> Tm -> Doc
prettyTm p = \case
  Iden x _        -> text x
  Lam bs t _      -> par p LowP $ prettyLam bs t
  App t u         -> par p AppP $ prettyTm AppP t <+> prettyTm AtomP u
  AppI t as _     -> par p AppP $ prettyTm AppP t <+> (braces . hjoin ", " $ map (prettyTy LowP) as)
  Let bs t _      -> par p LowP $ prettyLet bs t
  TyAnn t a       -> par p AnnP $ prettyTm P0 t <+> char ':' <+> prettyTy LowP a
  Hole _          -> char '_'
  IntLit n _ _    -> prettyIntLit n
  FloatLit n _ _  -> prettyFloatLit n
  CharLit c _ _   -> prettyCharLit c
  StringLit s _ _ -> prettyStringLit s
  Tup ts _ _      -> parens . hjoin ", " $ map (prettyTm LowP) ts
  ListLit ts _ _  -> brackets . hjoin ", " $ map (prettyTm LowP) ts
  UnOp op t _     -> prettyUnOp p op t
  BinOp op t u    -> prettyBinOp p op t u
  NullFunc f _ _  -> text (nullFuncName f)
  UnFunc f t _    -> prettyKwApp p (unFuncName f) [t]
  BinFunc f t u _ -> prettyKwApp p (binFuncName f) [t, u]
  If t u v _      -> par p LowP $ prettyIf t u v
  Split t xs u _  -> par p LowP $ prettySplit t xs u
  Switch bs _ _   -> par p BlockP $ prettySwitch bs
  Case t bs _ _   -> par p BlockP $ prettyCase t bs
  Do bs t _       -> par p LowP $ prettyDo bs t

-- | Pretty-print a top-level definition.
--
-- @since 1.0.0
prettyTopLevelDef :: Prec -> TopLevelDef -> Doc
prettyTopLevelDef p (TL x a t _ _) =
  par p LowP $ prettyTySig x a
            $$ nest 2 (char '=' <+> prettyTm LowP t) <> char ';'

----------------------------------------
-- Printing type declarations

prettyConstructor :: Prec -> Constructor -> Doc
prettyConstructor p (Constructor x a _) = par p LowP $ prettyTySig x a

prettyConstructors :: [Constructor] -> Doc
prettyConstructors = \case
  []      -> empty
  c : cs  -> space <> (char '=' <+> prettyConstructor LowP c
                    $$ vcat (map (\c -> char '|' <+> prettyConstructor LowP c) cs))

-- | Pretty-print a data type declaration.
--
-- @since 1.0.0
prettyDataDecl :: Prec -> DataDecl -> Doc
prettyDataDecl p (DD x xs cs _ _) =
  par p LowP $ text "data" <+> text x <+> hsep (map text xs)
            <> prettyConstructors cs <> char ';'

-- | Pretty-print a type declaration.
--
-- @since 1.0.0
prettyTypeDecl :: Prec -> TypeDecl -> Doc
prettyTypeDecl p = \case
  DataDecl dd -> prettyDataDecl p dd

------------------------------------------------------------
-- Printing programs

prettyProgram :: Prec -> Program -> Doc
prettyProgram p = par p LowP . vcat
                . map (endDef . either (prettyTypeDecl LowP) (prettyTopLevelDef LowP))
  where
    endDef doc = doc $$ text ""
