{-# LANGUAGE KindSignatures, LambdaCase #-}

-- | Types describing the presyntax.
module SFPL.Syntax.Raw.Types where

import Data.Kind
import SFPL.Base
import Text.Megaparsec.Pos (SourcePos)

------------------------------------------------------------
-- Common definitions

-- | Class for syntactic categories of the presyntax.
--
-- @since 1.0.0
class Raw (a :: Type) where
  -- | Get the span of an element.
  spanOf :: a -> Span

-- | Get the starting position of an element.
--
-- @since 1.0.0
begOf :: Raw a => a -> BegPos
begOf = fst . spanOf

-- | Get the ending position of an element.
--
-- @since 1.0.0
endOf :: Raw a => a -> EndPos
endOf = snd . spanOf

-- | Source position at the beginning of a syntactic element.
--
-- @since 1.0.0
type BegPos = SourcePos

-- | Source position one position after the end of a syntactic element.
--
-- @since 1.0.0
type EndPos = SourcePos

-- | Span of syntactic elements: starting and ending positions.
--
-- @since 1.0.0
type Span = (BegPos, EndPos)

------------------------------------------------------------
-- Types

-- | Types.
--
-- @since 1.0.0
data Ty
  = -- | A type identifier - type variable or type name.
    TyIden TyName BegPos
  | -- | Type hole (to be filled in by elaboration).
    THole BegPos
  | -- | Integer type.
    Int BegPos EndPos
  | -- | Floating pointer number type.
    Float BegPos EndPos
  | -- | Character type.
    Char BegPos EndPos
  | -- | Tuple type. A 0-component tuple is the unit type, and a 1-component tuple
    -- is just a type in parentheses.
    Tuple [Ty] BegPos EndPos
  | -- | Homogeneous list type.
    List Ty BegPos EndPos
  | -- | The real world.
    World Ty BegPos
  | -- | Type constructor application, with at least one parameter.
    TApp Ty TSpine
  | -- | Function type.
    Fun Ty Ty
  | -- | Universal quantification, with at least one binder.
    ForAll [TyName] Ty BegPos

-- | The parameters applied to a type constructor.
--
-- @since 1.0.0
type TSpine = [Ty]

------------------------------------------------------------
-- Patterns

-- | Arguments of a constructor pattern. An argument is either a field binding
-- or an implicit type variable binding.
--
-- @since 1.0.0
type CtrArgs = [Either Name TyName]

-- | Patterns.
--
-- @since 1.0.0
data Pattern
  = -- | An integer literal pattern.
    IntPat Integer BegPos EndPos
  | -- | A floating point literal pattern.
    FloatPat Double BegPos EndPos
  | -- | A character literal pattern.
    CharPat Char BegPos EndPos
  | -- | Tuple pattern (except 1-tuple).
    TuplePat [Name] BegPos EndPos
  | -- | Empty list pattern.
    EmptyListPat BegPos EndPos
  | -- | Cons pattern.
    ConsPat Name Name BegPos EndPos
  | -- | Data constructor pattern.
    CtrPat Name CtrArgs BegPos EndPos
  | -- | Wildcard pattern.
    WildcardPat BegPos

------------------------------------------------------------
-- Terms

-- | Built-in unary operators.
--
-- @since 1.0.0
data UnaryOp
  = -- | Plus sign.
    Plus
  | -- | Minus sign.
    Minus
  | -- | Bitwise "not".
    BNot
  | -- | Boolean "not".
    Not
  | -- | Pure operation (without effects).
    Pure

-- | Details about a unary operator: its precedence and symbol.
--
-- @since 1.0.0
unOpDetails :: UnaryOp -> (Prec, String)
unOpDetails = \case
  Plus    -> (P9, "+")
  Minus   -> (P9, "-")
  BNot    -> (P9, "~")
  Not     -> (P9, "!")
  Pure    -> (P9, "%")

-- | Built-in binary operators.
--
-- @since 1.0.0
data BinaryOp
  = -- | Addition of numbers.
    Add
  | -- | Subtraction of numbers.
    Sub
  | -- | Multiplication of numbers.
    Mul
  | -- | Division of numbers. If the operands are integers, performs integer division,
    -- otherwise floating point division.
    Div
  | -- | Exponentiation of numbers.
    Exp
  | -- | Equality.
    Eq
  | -- | "Not equal to" relation.
    Neq
  | -- | "Less than" relation.
    Lt
  | -- | "Less than or equal to" relation.
    Lte
  | -- | "Greater than" relation.
    Gt
  | -- | "Greater than or equal to" relation.
    Gte
  | -- | Bitwise "and".
    BAnd
  | -- | Bitwise "or".
    BOr
  | -- | Boolean "and".
    And
  | -- | Boolean "or".
    Or
  | -- | Cons operation on lists.
    Cons
  | -- | Sequencing.
    Seq

-- | Details about a binary operator: its precedence, associativity and symbol.
--
-- @since 1.0.0
binOpDetails :: BinaryOp -> (Prec, Assoc, String)
binOpDetails = \case
  Add   -> (P6, LeftAssoc , "+" )
  Sub   -> (P6, LeftAssoc , "-" )
  Mul   -> (P7, LeftAssoc , "*" )
  Div   -> (P7, LeftAssoc , "/" )
  Exp   -> (P8, RightAssoc, "^" )
  Eq    -> (P4, None      , "==")
  Neq   -> (P4, None      , "!=")
  Lt    -> (P4, None      , "<" )
  Lte   -> (P4, None      , "<=")
  Gt    -> (P4, None      , ">" )
  Gte   -> (P4, None      , ">=")
  BAnd  -> (P7, LeftAssoc , "|" )
  BOr   -> (P6, LeftAssoc , "&" )
  And   -> (P3, RightAssoc, "&&")
  Or    -> (P2, RightAssoc, "||")
  Cons  -> (P5, RightAssoc, "::")
  Seq   -> (P1, LeftAssoc , ">>")

-- | Built-in nullary functions (constants).
--
-- @since 1.0.0
data NullaryFunc
  = -- | Read a character from standard input.
    Read
  | -- | Get the next character from standard input, but don't consume it.
    Peek
  | -- | Query if end of input has been reached.
    IsEOF

-- | Get the name of a built-in nullary function (constant).
--
-- @since 1.0.0
nullFuncName :: NullaryFunc -> Name
nullFuncName = \case
  Read  -> kwRead
  Peek  -> kwPeek
  IsEOF -> kwIsEOF

-- | Built-in unary functions.
--
-- @since 1.0.0
data UnaryFunc
  = -- | Convert a number or character to an integer.
    ToInt
  | -- | Convert a number to floating point.
    ToFloat
  | -- | Convert an integer to a character according to the ASCII table.
    ToChar
  | -- | Throw an error.
    Error
  | -- | Put a character to standard output.
    Putc
  | -- | Print a value to standard output.
    Print

-- | Get the name of a built-in unary function.
--
-- @since 1.0.0
unFuncName :: UnaryFunc -> Name
unFuncName = \case
  ToInt   -> kwInt
  ToFloat -> kwFloat
  ToChar  -> kwChar
  Error   -> kwError
  Putc    -> kwPutc
  Print   -> kwPrint

-- | Built-in binary functions.
--
-- @since 1.0.0
data BinaryFunc
  = -- | Primitive equality.
    PrimEq
  | -- | Primitive "less than" relation.
    PrimLt

-- | Get the name of a built-in binary function.
--
-- @since 1.0.0
binFuncName :: BinaryFunc -> Name
binFuncName = \case
  PrimEq  -> kwPrimEq
  PrimLt  -> kwPrimLt

-- A binder in a lambda expression.
--
-- @since 1.0.0
data LamBind
  = -- | A term binding, which may or may not have a type signature.
    Expl Name (Maybe Ty)
  | -- | An implicit type binding.
    Impl TyName

-- | A local binding in a let or do expression, which may or may not have a type signature.
--
-- @since 1.0.0
type LocalBind = (Name, Maybe Ty, Tm)

-- | A branch of a switch expression.
--
-- @since 1.0.0
type SwitchBranch = (Tm, Tm)

-- | A branch of a case expression.
--
-- @since 1.0.0
type CaseBranch = (Pattern, Tm)

-- | Terms.
--
-- @since 1.0.0
data Tm
  = -- | An identifier - variable, top level definition or data constructor.
    Iden Name BegPos
  | -- | Lambda abstraction, with at least one binder.
    Lam [LamBind] Tm BegPos
  | -- | Application.
    App Tm Tm
  | -- | Implicit type application. The parameter list must not be empty.
    AppI Tm TSpine EndPos
  | -- | Let expression, with at least one local binding.
    Let [LocalBind] Tm BegPos
  | -- | Type annotation.
    TyAnn Tm Ty
  | -- | Hole. When the interpreter encounters a hole, it fails with
    -- an error instantly, showing the inferred type of the hole, 
    -- and the name and type of the bindings in scope at that moment.
    Hole BegPos
  | -- | An integer literal.
    IntLit Integer BegPos EndPos
  | -- | A floating point literal.
    FloatLit Double BegPos EndPos
  | -- | A character literal
    CharLit Char BegPos EndPos
  | -- | A string literal.
    StringLit String BegPos EndPos
  | -- | Tuples. A 0-component tuple is the unit, and a 1-component tuple
    -- is just a term in parentheses.
    Tup [Tm] BegPos EndPos
  | -- | A list literal.
    ListLit [Tm] BegPos EndPos
  | -- | A built-in unary operator.
    UnOp UnaryOp Tm BegPos
  | -- | A built-in binary operator.
    BinOp BinaryOp Tm Tm
  | -- | A built-in nullary function (constant).
    NullFunc NullaryFunc BegPos EndPos
  | -- | A built-in unary function.
    UnFunc UnaryFunc Tm BegPos
  | -- | A built-in binary function.
    BinFunc BinaryFunc Tm Tm BegPos
  | -- | If expression.
    If Tm Tm Tm BegPos
  | -- | Split expression.
    Split Tm [Name] Tm BegPos
  | -- | Switch expression.
    Switch [SwitchBranch] BegPos EndPos
  | -- | Case expression.
    Case Tm [CaseBranch] BegPos EndPos
  | -- | Do expression, with at least one local binding.
    Do [LocalBind] Tm BegPos

-- | A top-level definition.
--
-- @since 1.0.0
data TopLevelDef = TL Name Ty Tm BegPos EndPos

------------------------------------------------------------
-- Type declarations

-- | Data constructor declaration. A constructor has a name and a type.
--
-- @since 1.0.0
data Constructor = Constructor Name Ty BegPos

-- | Data type declaration, costisting of a type name, a list of type parameters
-- and a list of constructors, both of which may be empty.
data DataDecl = DD TyName [TyName] [Constructor] BegPos EndPos

-- | A type declaration. Currently only algebraic data types are supported.
data TypeDecl = DataDecl DataDecl

------------------------------------------------------------
-- Programs

-- | A program consists of a list of type declarations and top-level definitions.
-- An executable program must contain a top-level definition named @main@, with type 'World Unit'.
--
-- @since 1.0.0
type Program = [Either TypeDecl TopLevelDef]
