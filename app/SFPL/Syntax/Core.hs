{-# LANGUAGE DerivingVia #-}

-- | The core syntax.
module SFPL.Syntax.Core
  ( -- * Types
    Ty (..),
    TSpine,
    
    -- * Terms
    UnaryOp (..),
    unOpDetails,
    BinaryOp (..),
    binOpDetails,
    NullaryFunc (..),
    nullFuncName,
    UnaryFunc (..),
    unFuncName,
    BinaryFunc (..),
    binFuncName,
    CaseBranch,
    Tm (..),
    
    -- * Patterns
    CtrArgs,
    Pattern (..),
    
    -- * Printing
    TyPCxt,
    tyPCxt,
    prettyTy,
    showTyPrec,
    showTy,
    
    PatPCxt,
    patPCxt,
    prettyPat,
    showPatPrec,
    showPat,
    
    TmPCxt,
    tmPCxt,
    prettyTm,
    showTmPrec,
    showTm,
  )
  where

import SFPL.Syntax.Core.Types
import SFPL.Syntax.Core.Pretty
import SFPL.Syntax.Core.Instances
