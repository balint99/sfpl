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
    
    -- * Programs
    Program,
    
    -- * Printing
    TyPCxt,
    tyPCxt,
    prettyTy,
    showTyPrec,
    showTy,
    
    PatPCxt,
    patPCxt,
    prettyPat,
    
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
    
    DDPCxt,
    ddPCxt,
    prettyDataDecl,
    
    TDPCxt,
    tdPCxt,
    prettyTypeDecl,
    
    ProgPCxt,
    progPCxt,
    prettyProgram,
    showProgramPrec,
    showProgram,
  )
  where

import SFPL.Syntax.Core.Types
import SFPL.Syntax.Core.Pretty
import SFPL.Syntax.Core.Instances
