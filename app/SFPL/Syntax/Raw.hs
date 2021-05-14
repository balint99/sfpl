
-- | The presyntax of the language.
module SFPL.Syntax.Raw
  ( -- * Common definitions
    Raw (..),
    begOf,
    endOf,
    BegPos,
    EndPos,
    Span,
  
    -- * Types
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
    LamBind (..),
    LocalBind,
    SwitchBranch,
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
    prettyTy,
    prettyPat,
    prettyTm,
    prettyTopLevelDef,
    prettyDataDecl,
    prettyTypeDecl,
    prettyProgram,
  )
  where

import SFPL.Syntax.Raw.Types
import SFPL.Syntax.Raw.Pretty
import SFPL.Syntax.Raw.Instances
