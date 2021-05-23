{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}

-- | Instances for types needed for evaluation.
module SFPL.Eval.Instances where

import SFPL.Base
import SFPL.Eval.Types
import SFPL.Eval.Pretty
import SFPL.Syntax.Core

----------------------------------------
-- Utility

-- | @since 1.0.0
instance Num VTy where
  fromInteger = VTyVar . fromInteger
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined

deriving instance Eq VTy      -- ^ @since 1.0.0
deriving instance Eq TClosure -- ^ @since 1.0.0

----------------------------------------
-- Printing

deriving instance Show VTy      -- ^ @since 1.0.0
deriving instance Show TClosure -- ^ @since 1.0.0

deriving instance Show IOVal   -- ^ @since 1.0.0
deriving instance Show Val     -- ^ @since 1.0.0
deriving instance Show Closure -- ^ @since 1.0.0

-- | @since 1.0.0
instance Pretty CtrNames Val where
  prettyPrec = prettyVal
