{-# LANGUAGE LambdaCase #-}

-- | Types describing the core syntax.
module SFPL.Syntax.Core.Types where

import SFPL.Base

------------------------------------------------------------
-- Types

-- | Types.
--
-- @since 1.0.0
data Ty
  = -- | A type variable.
    TyVar Ix
  | -- | A data type.
    Data Lvl TSpine
  | -- | A metavariable.
    Meta Metavar TSpine
  | -- | A freshly inserted metavariable, applied to all bound type variables.
    FreshMeta Metavar
  | -- | Integer type.
    Int
  | -- | Floating point number type.
    Float
  | -- | Character type.
    Char
  | -- | Tuple type (except 1-tuples).
    Tuple [Ty]
  | -- | The real world.
    World Ty
  | -- | Function type.
    Fun Ty Ty
  | -- | Universal quantification.
    ForAll TyName Ty

-- | The parameters applied to a type constructor.
--
-- @since 1.0.0
type TSpine = [Ty]
  
------------------------------------------------------------
-- Terms

-- | Built-in unary operators.
--
-- @since 1.0.0
data UnaryOp
  = -- | Negation.
    Negate
  | -- | Bitwise "not".
    BNot
  | -- | Return in the IO monad.
    Pure

-- | Details about a unary operator: its precedence and symbol.
--
-- @since 1.0.0
unOpDetails :: UnaryOp -> (Prec, String)
unOpDetails = \case
  Negate  -> (P9, "-")
  BNot    -> (P9, "~")
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
  | -- | Bitwise "and".
    BAnd
  | -- | Bitwise "or".
    BOr

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
  BAnd  -> (P7, LeftAssoc , "|" )
  BOr   -> (P6, LeftAssoc , "&" )

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

-- | A branch of a case expression.
--
-- @since 1.0.0
type CaseBranch = (Pattern, Tm)

-- | Terms.
--
-- @since 1.0.0
data Tm
  = -- | A variable.
    Var Ix
  | -- | A top-level definition.
    TopLevel Lvl
  | -- | Lambda abstraction.
    Lam Name Ty Tm
  | -- | Implicit lambda for type variables.
    LamI TyName Tm
  | -- | Application.
    App Tm Tm
  | -- | Implicit type application.
    AppI Tm Ty
  | -- | Local definition.
    Let Name Ty Tm Tm
  | -- | An integer literal.
    IntLit Integer
  | -- | A floating point literal.
    FloatLit Double
  | -- | A character literal
    CharLit Char
  | -- | Tuple (except 1-tuple).
    Tup [Tm]
  | -- | Data constructor.
    Ctr Lvl
  | -- | A built-in unary operator.
    UnOp UnaryOp Tm
  | -- | A built-in binary operator.
    BinOp BinaryOp Tm Tm
  | -- | A built-in nullary function (constant).
    NullFunc NullaryFunc
  | -- | A built-in unary function.
    UnFunc UnaryFunc Tm
  | -- | A built-in binary function.
    BinFunc BinaryFunc Tm Tm
  | -- | Case expression.
    Case Tm [CaseBranch]
  | -- | Bind in the IO monad.
    Bind Name Ty Tm Tm

------------------------------------------------------------
-- Patterns

-- | Arguments of a constructor pattern. An argument is either a field binding
-- or an implicit type variable binding.
--
-- @since 1.0.0
type CtrArgs = [Either Name TyName]

-- | Patterns in a case expression.
--
-- @since 1.0.0
data Pattern
  = -- | An integer.
    PInt Integer
  | -- | A floating point number.
    PFloat Double
  | -- | A character.
    PChar Char
  | -- | A tuple (except 1-tuple).
    PTuple [Name]
  | -- | A data constructor.
    PCtr Lvl CtrArgs
  | -- | A wildcard.
    PWildcard
