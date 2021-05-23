{-# LANGUAGE KindSignatures, PatternSynonyms #-}

-- | Type definitions needed for evaluation.
module SFPL.Eval.Types where

import Control.Monad.Reader
import Data.Kind
import Data.Array.IArray hiding (Ix)
import SFPL.Base
import SFPL.Elab.Metacontext (SomeMetas)
import SFPL.Syntax.Core (Ty, Tm)

------------------------------------------------------------
-- Types

-- | Type values.
--
-- @since 1.0.0
data VTy
  = -- | Type variables.
    VTyVar Lvl
  | -- | Data types.
    VData Lvl VTSpine
  | -- | Metavariables.
    VMeta Metavar VTSpine
  | -- | Integer type.
    VTInt
  | -- | Floating point number type.
    VTFloat
  | -- | Character type.
    VTChar
  | -- | Tuple type.
    VTTuple [VTy]
  | -- | World type.
    VWorld VTy
  | -- | Function type.
    VFun VTy VTy
  | -- | Universal quantification.
    VForAll TyName TClosure

-- | Unit type.
--
-- @since 1.0.0
pattern VUnit :: VTy
pattern VUnit = VTTuple []

-- | Type values applied to a type constructor or metavariable.
--
-- @since 1.0.0
type VTSpine = [VTy]

-- | Type evaluation environment.
--
-- @since 1.0.0
type TEnv = [VTy]

-- | Type closure.
--
-- @since 1.0.0
data TClosure = TClosure TEnv Ty

------------------------------------------------------------
-- Terms

-- | IO actions which have not yet been performed.
--
-- @since 1.0.0
data IOVal
  = -- | Pure action (without effects).
    VPure Env Tm
  | -- | Bind action (sequential execution).
    VBind Env Tm Tm
  | -- | Read a character from standard input.
    VRead
  | -- | Get the next character from standard input without consuming it.
    VPeek
  | -- | Query if end of input has been reached.
    VIsEOF
  | -- | Put a character to standard output.
    VPutc Env Tm
  | -- | Print a value nicely to standard output.
    VPrint Env Tm

-- | Term values.
--
-- @since 1.0.0
data Val
  = -- | Lambda.
    VLam Closure
  | -- | Integers.
    VInt Integer
  | -- | Floating point numbers.
    VFloat Double
  | -- | Characters.
    VChar Char
  | -- | Tuples.
    VTuple [Val]
  | -- | Data constructors.
    VCtr Lvl VSpine
  | -- | An IO action.
    VIO IOVal

-- | Unit value.
--
-- @since 1.0.0
pattern VTt :: Val
pattern VTt = VTuple []

-- | Values applied to a data constructor.
--
-- @since 1.0.0
type VSpine = [Val]

-- | Term evaluation environment.
--
-- @since 1.0.0
type Env = [Val]

-- | Closure.
--
-- @since 1.0.0
data Closure = Closure Env Tm

------------------------------------------------------------
-- Other definitions

-- | Top level definitions.
--
-- @since 1.0.0
type TopLevel = Array Lvl Tm

-- | Names of data constructors used for printing.
--
-- @since 1.0.0
type CtrNames = Array Lvl Name

-- | Evaluation context: top-level definitions and constructor names.
--
-- @since 1.0.0
type EvalCxt = (TopLevel, CtrNames)

-- | An error that can occur during evaluation.
--
-- @since 1.0.0
type EvalError = String

-- | Type evaluation monad.
--
-- @since 1.0.0
type EvalT = (->) SomeMetas

-- | Pure term evaluation monad.
--
-- @since 1.0.0
type EvalP = ReaderT EvalCxt (Either EvalError)

------------------------------------------------------------
-- Class

-- | Type class for monads in which
-- effectful evaluation can be carried out.
--
-- @since 1.0.0
class Monad m => MonadRun (m :: Type -> Type) where
  -- | Get the evaluation context.
  getEvalCxt :: m EvalCxt
  
  -- | Throw an evaluation error.
  throwEvalError :: EvalError -> m a
  
  -- | Read a character from standard input.
  -- If end of input has been reached, should return 'Nothing'.
  readChar :: m (Maybe Char)
  
  -- | Get the next character from standard input without consuming it.
  -- If end of input has been reached, should return 'Nothing'.
  peekChar :: m (Maybe Char)
  
  -- | Write a string to standard output.
  write :: String -> m ()
