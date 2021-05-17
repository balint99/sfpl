
-- | Errors that can occur during elaboration.
module SFPL.Elab.Error.Types where

import SFPL.Base
import SFPL.Elab.Context
import SFPL.Elab.Metacontext
import SFPL.Syntax.Raw.Types

-- | Any kind of elaboration error.
--
-- @since 1.0.0
data ElabError = ElabError
  { -- | State of elaboration context at the point when the error occured.
    elabErrorCxt :: ElabCxt
  , -- | State of metacontext at the point when the error occured.
    elabErrorMetas :: SomeMetas
  , -- | The precise span in the source where the error occured.
    elabErrorSpan :: Span
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
    -- Includes the unknown name and expected syntactic category.
    NotInScopeError Name SyntacticCategory
  | -- | Multiple declarations of an identifier.
    -- Includes the name, and the source position of the previous declaration.
    MultipleDeclarationsError Name BegPos
  | -- | A unification error. Includes the expected and inferred types.
    UnificationError Ty Ty
  {-| -- | A metavariable is ambiguous.
    -- Includes the identifier of the metavariable.
    AmbiguousMeta Metavar-}
  | -- | An overloaded operator or built-in function is ambiguous.
    -- Includes the type of the overloaded symbol.
    AmbiguousOverloading OverloadType
  | -- | A term hole was encountered.
    -- Includes the expected type of the hole.
    HoleError Ty

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
