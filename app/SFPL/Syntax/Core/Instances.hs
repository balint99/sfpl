{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}

-- | Instances for the types describing the core syntax.
module SFPL.Syntax.Core.Instances where

import SFPL.Base
import SFPL.Syntax.Core.Types
import SFPL.Syntax.Core.Pretty

----------------------------------------
-- Types

-- | @since 1.0.0
instance Pretty TyPCxt Ty where
  prettyPrec = prettyTy

----------------------------------------
-- Patterns

-- | @since 1.0.0
instance Pretty PatPCxt Pattern where
  prettyPrec = prettyPat

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
  prettyPrec = prettyTm
