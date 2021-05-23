{-# LANGUAGE LambdaCase #-}

-- | Errors that can occur during elaboration.
module SFPL.Elab.Error.Types where

import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Metacontext
import SFPL.Eval.Types
import SFPL.Syntax.Raw.Types
import SFPL.Syntax.Raw.Instances

-- | Any kind of elaboration error.
--
-- @since 1.0.0
data ElabError = ElabError
  { -- | State of elaboration context at the point when the error occured.
    elabErrorCxt :: ElabCxt
  , -- | The precise span in the source where the error occured.
    -- 'Nothing' means there is no location info.
    elabErrorSpan :: Maybe Span
  , -- | The type of the error, which includes further details.
    elabErrorType :: ElabErrorType
  , -- | Optional additional error items providing details about the error.
    elabErrorItems :: [ElabErrorItem]
  }

-- | The type of an elaboration error with further details.
--
-- @since 1.0.0
data ElabErrorType
  = -- | An identifier is not in scope.
    -- Includes the unknown name and expected syntactic categories (at least 1).
    NotInScopeError Name [SyntacticCategory]
  | -- | Multiple declarations of an identifier.
    -- Includes the name, and the source position of the previous declaration.
    MultipleDeclarationsError Name BegPos
  | -- | A type is malformed (ill-kinded).
    -- | Includes the malformed type and the reason for the failure.
    MalformedTypeError Ty MalformedTypeErrorReason
  | -- | A type hole was found in an invalid place.
    -- Includes the type and the place.
    InvalidTypeHoleError Ty TypeHolePlace
  | -- | The return type of a data constructor doesn't match the type
    -- the type that defines it.
    -- Includes the name of the constructor and the return type.
    BadConstructorType Name Ty
  | -- | A constructor pattern is malformed.
    -- | Includes the malformed pattern.
    MalformedPatternError Pattern
  | -- | A unification error.
    -- Includes the expected and inferred type, and the reason for the failure.
    UnificationError VTy VTy UnificationErrorReason
  | -- | A function was applied to an implicit type argument.
    -- Includes the type of the function and the applied type argument.
    ImplicitApplicationError VTy Ty
  | -- | An overloaded operator or built-in function does not support
    -- the type of one of its operands.
    -- Includes the type of the overloaded symbol and the unsupported type.
    TypeNotSupportedError OverloadType VTy
  | -- | An overloaded operator or built-in function is ambiguous.
    -- Includes the type of the overloaded symbol.
    AmbiguousOverloadingError OverloadType
  | -- | A term hole was encountered.
    -- Includes the expected type of the hole.
    HoleError VTy
  | -- | The main function was not found.
    MainNotFoundError
  | -- | The main function was defined as a data constructor.
    IllegalMainError

-- | The types of syntactic elements which can generate a scope error.
--
-- @since 1.0.0
data SyntacticCategory
  = -- | A type.
    SCType
  | -- | A top-level definition or bound variable.
    SCVariable
  | -- | A data constructor.
    SCConstructor

-- | Get the name of a syntactic category.
--
-- @since 1.0.0
scName :: SyntacticCategory -> String
scName = \case
  SCType        -> "type"
  SCVariable    -> "variable"
  SCConstructor -> "data constructor"

-- | The reason why a type was malformed.
--
-- @since 1.0.0
data MalformedTypeErrorReason
  = -- | A type other than a type constructor was applied to arguments.
    IllegalApplication
  | -- | A data type was not supplied the appropriate number of type parameters.
    -- Includes the name of the type, expected and actual number of parameters.
    BadDataApplication TyName Int Int

-- | The place of a type signature when type holes are not allowed.
--
-- @since 1.0.0
data TypeHolePlace
  = -- | Type signature of a top-level definition.
    THPTopLevelDef
  | -- | Type signature of a data constructor.
    THPConstructor

-- | The reason why a unification failed.
--
-- @since 1.0.0
data UnificationErrorReason
  = -- | There was a rigid mismatch between constructors.
    RigidMismatch
  | -- | The spine did not consist of distinct bound variables.
    InvalidSpine
  | -- | The right hand contained a free variable not contained in the spine.
    EscapingVariable Ix
  | -- | The metavariable solved for occurred recursively in the equation.
    Occurs Metavar

-- | The type of an overloaded entity.
--
-- @since 1.0.0
data OverloadType
  = -- | An operator with its symbol.
    OTOperator String
  | -- | A built-in function with its name.
    OTFunction Name

-- | Additional error items providing further information regarding an error.
--
-- @since 1.0.0
data ElabErrorItem
  = -- | A list of local bindings and their types.
    Bindings
