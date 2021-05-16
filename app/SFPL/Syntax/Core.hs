{-# LANGUAGE DerivingVia #-}

-- | The core syntax.
module SFPL.Syntax.Core
  ( -- * Types
    Ty (..),
    TSpine,
    
    -- * Patterns
    CtrArgs,
    Pattern (..),
    
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
    TopLevelDef (..),
    
    -- * Type declarations
    Constructor (..),
    DataDecl (..),
    TypeDecl (..),
    
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
    
    TLPCxt,
    tlPCxt,
    prettyTopLevelDef,
    showTopLevelDefPrec,
    showTopLevelDef,
    
    CtrPCxt,
    ctrPCxt,
    prettyConstructor,
    showConstructorPrec,
    showConstructor,
    
    DDPCxt,
    ddPCxt,
    prettyDataDecl,
    showDataDeclPrec,
    showDataDecl,
    
    TDPCxt,
    tdPCxt,
    prettyTypeDecl,
    showTypeDeclPrec,
    showTypeDecl,
  )
  where

import SFPL.Syntax.Core.Types
import SFPL.Syntax.Core.Pretty
import SFPL.Syntax.Core.Instances
