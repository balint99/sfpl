{-# LANGUAGE FlexibleInstances, LambdaCase, StandaloneDeriving #-}

-- | Instances for the types describing the presyntax.
module SFPL.Syntax.Raw.Instances where

import SFPL.Base
import SFPL.Syntax.Raw.Types
import SFPL.Syntax.Raw.Pretty
import SFPL.Utils (addCols)
import Text.Megaparsec.Pos

----------------------------------------
-- Types

tySpan :: Ty -> Span
tySpan = \case
  TyIden x beg end  -> (beg, end)
  THole beg         -> (beg, beg `addCols` 1)
  Int beg end       -> (beg, end)
  Float beg end     -> (beg, end)
  Char beg end      -> (beg, end)
  Tuple _ beg end   -> (beg, end)
  List _ beg end    -> (beg, end)
  World a beg       -> (beg, endOf a)
  TApp a sp         -> (begOf a, endOf $ last sp)
  Fun a b           -> (begOf a, endOf b)
  ForAll _ a beg    -> (beg, endOf a)

-- | @since 1.0.0
instance Raw Ty where
  spanOf = tySpan

-- | @since 1.0.0
instance Pretty' Ty where
  prettyPrec' = prettyTy

-- | @since 1.0.0
instance Show Ty where
  showsPrec p a = showString $ showPrettyPrec' (intToPrec p) a

deriving instance Eq Ty -- ^ @since 1.0.0

----------------------------------------
-- Patterns

patSpan :: Pattern -> Span
patSpan = \case
  IntPat _ beg end      -> (beg, end)
  FloatPat _ beg end    -> (beg, end)
  CharPat _ beg end     -> (beg, end)
  TuplePat _ beg end    -> (beg, end)
  EmptyListPat beg end  -> (beg, end)
  ConsPat _ _ beg end   -> (beg, end)
  CtrPat _ _ beg end    -> (beg, end)
  WildcardPat beg       -> (beg, beg `addCols` 1)

-- | @since 1.0.0
instance Raw Pattern where
  spanOf = patSpan

-- | @since 1.0.0
instance Pretty' Pattern where
  prettyPrec' = prettyPat

-- | @since 1.0.0
instance Show Pattern where
  showsPrec p pat = showString $ showPrettyPrec' (intToPrec p) pat

deriving instance Eq Pattern -- ^ @since 1.0.0
  
----------------------------------------
-- Terms

deriving instance Show    UnaryOp -- ^ @since 1.0.0
deriving instance Eq      UnaryOp -- ^ @since 1.0.0
deriving instance Enum    UnaryOp -- ^ @since 1.0.0
deriving instance Bounded UnaryOp -- ^ @since 1.0.0

deriving instance Show    BinaryOp -- ^ @since 1.0.0
deriving instance Eq      BinaryOp -- ^ @since 1.0.0
deriving instance Enum    BinaryOp -- ^ @since 1.0.0
deriving instance Bounded BinaryOp -- ^ @since 1.0.0

deriving instance Show    NullaryFunc -- ^ @since 1.0.0
deriving instance Eq      NullaryFunc -- ^ @since 1.0.0
deriving instance Enum    NullaryFunc -- ^ @since 1.0.0
deriving instance Bounded NullaryFunc -- ^ @since 1.0.0

deriving instance Show    UnaryFunc -- ^ @since 1.0.0
deriving instance Eq      UnaryFunc -- ^ @since 1.0.0
deriving instance Enum    UnaryFunc -- ^ @since 1.0.0
deriving instance Bounded UnaryFunc -- ^ @since 1.0.0

deriving instance Show    BinaryFunc -- ^ @since 1.0.0
deriving instance Eq      BinaryFunc -- ^ @since 1.0.0
deriving instance Enum    BinaryFunc -- ^ @since 1.0.0
deriving instance Bounded BinaryFunc -- ^ @since 1.0.0

deriving instance Show LamBind -- ^ @since 1.0.0
deriving instance Eq   LamBind -- ^ @since 1.0.0

tmSpan :: Tm -> Span
tmSpan = \case
  Iden x beg end      -> (beg, end)
  Lam _ t beg         -> (beg, endOf t)
  App t u             -> (begOf t, endOf u)
  AppI t _ end        -> (begOf t, end)
  Let _ t beg         -> (beg, endOf t)
  TyAnn t a           -> (begOf t, endOf a)
  Hole beg            -> (beg, beg `addCols` 1)
  IntLit _ beg end    -> (beg, end)
  FloatLit _ beg end  -> (beg, end)
  CharLit _ beg end   -> (beg, end)
  StringLit _ beg end -> (beg, end)
  Tup _ beg end       -> (beg, end)
  ListLit _ beg end   -> (beg, end)
  UnOp _ t beg        -> (beg, endOf t)
  BinOp _ t u         -> (begOf t, endOf u)
  NullFunc _ beg end  -> (beg, end)
  UnFunc _ t beg      -> (beg, endOf t)
  BinFunc _ _ u beg   -> (beg, endOf u)
  If _ _ v beg        -> (beg, endOf v)
  Split _ _ u beg     -> (beg, endOf u)
  Switch _ beg end    -> (beg, end)
  Case _ _ beg end    -> (beg, end)
  Do _ t beg          -> (beg, endOf t)

-- | @since 1.0.0
instance Raw Tm where
  spanOf = tmSpan

-- | @since 1.0.0
instance Pretty' Tm where
  prettyPrec' = prettyTm

-- | @since 1.0.0
instance Show Tm where
  showsPrec p t = showString $ showPrettyPrec' (intToPrec p) t

deriving instance Eq Tm -- ^ @since 1.0.0

topLevelDefSpan :: TopLevelDef -> Span
topLevelDefSpan (TL _ _ _ beg end) = (beg, end)

-- | @since 1.0.0
instance Raw TopLevelDef where
  spanOf = topLevelDefSpan

-- | @since 1.0.0
instance Pretty' TopLevelDef where
  prettyPrec' = prettyTopLevelDef

-- | @since 1.0.0
instance Show TopLevelDef where
  showsPrec p tl = showString $ showPrettyPrec' (intToPrec p) tl

deriving instance Eq TopLevelDef -- ^ @since 1.0.0

----------------------------------------
-- Type declarations

constructorSpan :: Constructor -> Span
constructorSpan (Constructor _ a beg) = (beg, endOf a)

-- | @since 1.0.0
instance Raw Constructor where
  spanOf = constructorSpan

-- | @since 1.0.0
instance Pretty' Constructor where
  prettyPrec' = prettyConstructor

-- | @since 1.0.0
instance Show Constructor where
  showsPrec p ctr = showString $ showPrettyPrec' (intToPrec p) ctr

deriving instance Eq Constructor -- ^ @since 1.0.0

dataDeclSpan :: DataDecl -> Span
dataDeclSpan (DD _ _ _ beg end) = (beg, end)

-- | @since 1.0.0
instance Raw DataDecl where
  spanOf = dataDeclSpan

-- | @since 1.0.0
instance Pretty' DataDecl where
  prettyPrec' = prettyDataDecl

-- | @since 1.0.0
instance Show DataDecl where
  showsPrec p dd = showString $ showPrettyPrec' (intToPrec p) dd

deriving instance Eq DataDecl -- ^ @since 1.0.0

typeDeclSpan :: TypeDecl -> Span
typeDeclSpan = \case
  DataDecl dd -> dataDeclSpan dd

-- | @since 1.0.0
instance Raw TypeDecl where
  spanOf = typeDeclSpan

-- | @since 1.0.0
instance Pretty' TypeDecl where
  prettyPrec' = prettyTypeDecl

-- | @since 1.0.0
instance Show TypeDecl where
  showsPrec p td = showString $ showPrettyPrec' (intToPrec p) td

deriving instance Eq TypeDecl -- ^ @since 1.0.0

----------------------------------------
-- Programs

-- | @since 1.0.0
instance {-# OVERLAPPING #-} Pretty' Program where
  prettyPrec' = prettyProgram

-- | @since 1.0.0
instance {-# OVERLAPPING #-} Show Program where
  showsPrec p pr = showString $ showPrettyPrec' (intToPrec p) pr
