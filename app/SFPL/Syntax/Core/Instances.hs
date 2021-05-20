{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}

-- | Instances for the types describing the core syntax.
module SFPL.Syntax.Core.Instances where

import SFPL.Base
import SFPL.Syntax.Core.Types
import SFPL.Syntax.Core.Pretty

----------------------------------------
-- Types

-- | @since 1.0.0
instance Num Ty where
  fromInteger = TyVar . fromInteger
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined

-- | @since 1.0.0
instance Pretty TyPCxt Ty where
  prettyPrec = prettyTy True

deriving instance Show Ty -- ^ @since 1.0.0

----------------------------------------
-- Patterns

-- | @since 1.0.0
instance Pretty PatPCxt Pattern where
  prettyPrec = prettyPat

deriving instance Show Pattern -- ^ @since 1.0.0

----------------------------------------
-- Terms

deriving instance Show    UnaryOp -- ^ @since 1.0.0
deriving instance Enum    UnaryOp -- ^ @since 1.0.0
deriving instance Bounded UnaryOp -- ^ @since 1.0.0

deriving instance Show    BinaryOp -- ^ @since 1.0.0
deriving instance Enum    BinaryOp -- ^ @since 1.0.0
deriving instance Bounded BinaryOp -- ^ @since 1.0.0

deriving instance Show    NullaryFunc -- ^ @since 1.0.0
deriving instance Enum    NullaryFunc -- ^ @since 1.0.0
deriving instance Bounded NullaryFunc -- ^ @since 1.0.0

deriving instance Show    UnaryFunc -- ^ @since 1.0.0
deriving instance Enum    UnaryFunc -- ^ @since 1.0.0
deriving instance Bounded UnaryFunc -- ^ @since 1.0.0

deriving instance Show    BinaryFunc -- ^ @since 1.0.0
deriving instance Enum    BinaryFunc -- ^ @since 1.0.0
deriving instance Bounded BinaryFunc -- ^ @since 1.0.0

-- | @since 1.0.0
instance Pretty TmPCxt Tm where
  prettyPrec = prettyTm True

deriving instance Show Tm -- ^ @since 1.0.0

-- | @since 1.0.0
instance Pretty TLPCxt TopLevelDef where
  prettyPrec = prettyTopLevelDef True

deriving instance Show TopLevelDef -- ^ @since 1.0.0

----------------------------------------
-- Type declarations

-- | @since 1.0.0
instance Pretty CtrPCxt Constructor where
  prettyPrec = prettyConstructor

deriving instance Show Constructor -- ^ @since 1.0.0

-- | @since 1.0.0
instance Pretty DDPCxt DataDecl where
  prettyPrec = prettyDataDecl

deriving instance Show DataDecl -- ^ @since 1.0.0

-- | @since 1.0.0
instance Pretty TDPCxt TypeDecl where
  prettyPrec = prettyTypeDecl

deriving instance Show TypeDecl -- ^ @since 1.0.0

----------------------------------------
-- Programs

-- | @since 1.0.0
instance Pretty ProgPCxt Program where
  prettyPrec = prettyProgram True
