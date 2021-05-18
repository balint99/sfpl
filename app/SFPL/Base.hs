{-# LANGUAGE DerivingVia, FlexibleInstances, KindSignatures,
             LambdaCase, MultiParamTypeClasses #-}

-- | Common definitions needed in various modules.
module SFPL.Base
  ( -- * Type synonyms
    Name,
    TyName,
    Keyword,
    
    -- * Newtypes
    Ix (..),
    Lvl (..),
    lvl2Ix,
    Metavar (..),
    
    -- * Data types
    Prec (..),
    intToPrec,
    Assoc (..),
    
    -- * Classes
    Pretty (..),
    showPrettyPrec,
    showPretty,
    showPrettyPrec1,
    showPretty1,
    
    Pretty' (..),
    showPrettyPrec',
    showPretty',
    showPrettyPrec1',
    showPretty1',
  
    -- * Constants
    keywords,
    
    -- ** Keywords for types
    kwInt,
    kwFloat,
    kwChar,
    
    -- ** Keywords from primitive operations
    kwPrimEq,
    kwPrimLt,
    kwError,
    kwRead,
    kwPeek,
    kwIsEOF,
    kwPutc,
    kwPrint,
    
    -- ** Desugaring
    dsEqInt,
    dsEqFloat,
    dsEqChar,
    dsNeqInt,
    dsNeqFloat,
    dsNeqChar,
    dsLtInt,
    dsLtFloat,
    dsLtChar,
    dsLteInt,
    dsLteFloat,
    dsLteChar,
    dsGtInt,
    dsGtFloat,
    dsGtChar,
    dsGteInt,
    dsGteFloat,
    dsGteChar,
    dsNot,
    dsAnd,
    dsOr,
    dsIfThenElse,
    dsNil,
    dsCons,
  )
  where

import qualified Data.Ix as Ix
import Data.Hashable
import Data.Kind
import Text.PrettyPrint

------------------------------------------------------------
-- Type synonyms

-- | Names for term identifiers.
--
-- @since 1.0.0
type Name = String

-- | Names for type identifiers.
type TyName = Name

-- | Keywords.
type Keyword = String

------------------------------------------------------------
-- Newtypes

-- | De-Bruijn indices.
--
-- @since 1.0.0
newtype Ix = Ix { unIx :: Int }
  deriving
    ( Eq    -- ^ @since 1.0.0
    , Show  -- ^ @since 1.0.0
    , Num   -- ^ @since 1.0.0
    ) via Int

-- | De-Bruijn levels.
--
-- @since 1.0.0
newtype Lvl = Lvl { unLvl :: Int }
  deriving
    ( Eq        -- ^ @since 1.0.0
    , Ord       -- ^ @since 1.0.0
    , Enum      -- ^ @since 1.0.0
    , Show      -- ^ @since 1.0.0
    , Num       -- ^ @since 1.0.0
    , Ix.Ix     -- ^ @since 1.0.0
    , Hashable  -- ^ @since 1.0.0
    ) via Int

-- | Convert de-Bruijn level to index.
--
-- @since 1.0.0
lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl n) (Lvl l) = Ix (n - l - 1)

-- | Metavariables.
--
-- @since 1.0.0
newtype Metavar = Metavar { unMetavar :: Int }
  deriving
    ( Eq       -- ^ @since 1.0.0
    , Ord      -- ^ @since 1.0.0
    , Show     -- ^ @since 1.0.0
    , Num      -- ^ @since 1.0.0
    , Ix.Ix    -- ^ @since 1.0.0
    , Hashable -- ^ @since 1.0.0
    ) via Int

------------------------------------------------------------
-- Data types

-- | Operator precedences. 
--
-- @since 1.0.0
data Prec
  -- | Precedence of built-in mixfix expressions. This is the lowest precedence.
  = LowP
  -- | Precedence of type annotations.
  | AnnP
  | P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
  -- | Precedence of block expressions.
  | BlockP
  -- | Precedence of function application.
  | AppP
  -- | Precedence of atoms.
  | AtomP
  deriving
    ( Eq      -- ^ @since 1.0.0
    , Ord     -- ^ @since 1.0.0
    , Enum    -- ^ @since 1.0.0
    , Show    -- ^ @since 1.0.0
    , Bounded -- ^ @since 1.0.0
    )

-- | Convert an integer precedence to a 'Prec' value.
--
-- @since 1.0.0
intToPrec :: Int -> Prec
intToPrec = \case
  0  -> LowP
  1  -> P1
  2  -> P2
  3  -> P3
  4  -> P4
  5  -> P5
  6  -> P6
  7  -> P7
  8  -> P8
  9  -> P9
  10 -> AppP
  11 -> AtomP
  _  -> error "invalid precedence"

-- | Associativity of a binary operator.
--
-- @since 1.0.0
data Assoc
  -- | Non-associative.
  = None
  -- | Left associative.
  | LeftAssoc
  -- | Right associative.
  | RightAssoc

------------------------------------------------------------
-- Classes

-- | Types whose values can be pretty-printed, using extra information from the context @cxt@.
--
-- @since 1.0.0
class Pretty (cxt :: Type) (a :: Type) where
  -- | Pretty-print a value in a given context. @prettyPrec p cxt x@ prints the value of @x@
  -- in the current printing precedence context @p@, using the information in @cxt@.
  --
  -- @since 1.0.0
  prettyPrec :: Prec -> cxt -> a -> Doc
  prettyPrec _ = pretty
  
  -- | Pretty-print a value in the topmost context.
  -- Equivalent to 'prettyPrec LowP'.
  --
  -- @since 1.0.0
  pretty :: cxt -> a -> Doc
  pretty = prettyPrec LowP
  
  {-# MINIMAL prettyPrec | pretty #-}

-- Default instance
instance {-# OVERLAPPABLE #-} Pretty' a => Pretty () a where
  prettyPrec p _ = prettyPrec' p
  pretty _ = pretty'

-- | Pretty-print a value in the given context and render it using the default style.
--
-- @since 1.0.0
showPrettyPrec :: Pretty cxt a => Prec -> cxt -> a -> String
showPrettyPrec p cxt a = render $ prettyPrec p cxt a

-- | Pretty-print a value and render it using the default style.
--
-- @since 1.0.0
showPretty :: Pretty cxt a => cxt -> a -> String
showPretty cxt a = render $ pretty cxt a

-- | Pretty-print a value and render it on one line in the given context.
--
-- @since 1.0.0
showPrettyPrec1 :: Pretty cxt a => Prec -> cxt -> a -> String
showPrettyPrec1 p cxt a = renderStyle (style {mode = OneLineMode}) $ prettyPrec p cxt a

-- | Pretty-print a value and render it on one line.
--
-- @since 1.0.0
showPretty1 :: Pretty cxt a => cxt -> a -> String
showPretty1 cxt a = renderStyle (style {mode = OneLineMode}) $ pretty cxt a

-- | Types whose values can be pretty-printed without using extra information.
-- 
-- @since 1.0.0
class Pretty' (a :: Type) where
  -- | Pretty-print a value in a given context. @prettyPrec p x@ prints the value of @x@
  -- in the current printing precedence context @p@.
  --
  -- @since 1.0.0
  prettyPrec' :: Prec -> a -> Doc
  prettyPrec' _ = pretty'
  
  -- | Pretty-print a value in the topmost context.
  -- Equivalent to 'prettyPrec\' LowP'.
  --
  -- @since 1.0.0
  pretty' :: a -> Doc
  pretty' = prettyPrec' LowP
  
  {-# MINIMAL prettyPrec' | pretty' #-}

-- | Pretty-print a value and render it in the given context using the default style.
--
-- @since 1.0.0
showPrettyPrec' :: Pretty' a => Prec -> a -> String
showPrettyPrec' p x = render $ prettyPrec' p x

-- | Pretty-print a value and render it using the default style.
--
-- @since 1.0.0
showPretty' :: Pretty' a => a -> String
showPretty' a = render $ pretty' a

-- | Pretty-print a value and render it on one line in the given context.
--
-- @since 1.0.0
showPrettyPrec1' :: Pretty' a => Prec -> a -> String
showPrettyPrec1' p a = renderStyle (style {mode = OneLineMode}) $ prettyPrec' p a

-- | Pretty-print a value and render it on one line.
--
-- @since 1.0.0
showPretty1' :: Pretty' a => a -> String
showPretty1' a = renderStyle (style {mode = OneLineMode}) $ pretty' a

------------------------------------------------------------
-- Constants

-- | The list of keywords of the language.
--
-- @since 1.0.0
keywords :: [Keyword]
keywords =
  [ kwInt, kwFloat, kwChar, kwPrimEq, kwPrimLt
  , kwError, kwRead, kwPeek, kwIsEOF, kwPutc, kwPrint
  , "let", "in", "if", "then", "else", "switch"
  , "split", "as", "case", "of", "do", "_", "data" ]

----------------------------------------
-- Keywords for types

-- | Integer type.
kwInt :: Keyword
kwInt = "int"

-- | Floating point type.
kwFloat :: Keyword
kwFloat = "float"

-- | Character type.
kwChar :: Keyword
kwChar = "char"

----------------------------------------
-- Keywords for primitive operations.

-- | Equals.
kwPrimEq :: Keyword
kwPrimEq = "__eq"

-- | Less than.
kwPrimLt :: Keyword
kwPrimLt = "__lt"

-- | Throw an error.
kwError :: Keyword
kwError = "__error"

-- | Read a character from standard input.
kwRead :: Keyword
kwRead = "__read"

-- | Get the next character from standard input.
kwPeek :: Keyword
kwPeek = "__peek"

-- | Query if end of file is reached.
kwIsEOF :: Keyword
kwIsEOF = "__is_eof"

-- | Print a character to standard output.
kwPutc :: Keyword
kwPutc = "__putc"

-- | Print a value nicely to standard output.
kwPrint :: Keyword
kwPrint = "__print"

----------------------------------------
-- Desugaring

-- | "Equals" for integers.
dsEqInt :: String
dsEqInt = "eqInt"

-- | "Equals" for floating point numbers.
dsEqFloat :: String
dsEqFloat = "eqFloat"

-- | "Equals" for characters.
dsEqChar :: String
dsEqChar = "eqChar"

-- | "Not equals" for integers.
dsNeqInt :: String
dsNeqInt = "neqInt"

-- | "Not equals" for floating point numbers.
dsNeqFloat :: String
dsNeqFloat = "neqFloat"

-- | "Not equals" for characters.
dsNeqChar :: String
dsNeqChar = "neqChar"

-- | "Less than" for integers.
dsLtInt :: String
dsLtInt = "ltInt"

-- | "Less than" for floating point numbers.
dsLtFloat :: String
dsLtFloat = "ltFloat"

-- | "Less than" for characters.
dsLtChar :: String
dsLtChar = "ltChar"

-- | "Less than or equals" for integers.
dsLteInt :: String
dsLteInt = "lteInt"

-- | "Less than or equals" for floating point numbers.
dsLteFloat :: String
dsLteFloat = "lteFloat"

-- | "Less than or equals" for characters.
dsLteChar :: String
dsLteChar = "lteChar"

-- | "Greater than" for integers.
dsGtInt :: String
dsGtInt = "gtInt"

-- | "Greater than" for floating point numbers.
dsGtFloat :: String
dsGtFloat = "gtFloat"

-- | "Greater than" for characters.
dsGtChar :: String
dsGtChar = "gtChar"

-- | "Greater than or equals" for integers.
dsGteInt :: String
dsGteInt = "gteInt"

-- | "Greater than or equals" for floating point numbers.
dsGteFloat :: String
dsGteFloat = "gteFloat"

-- | "Greater than or equals" for characters.
dsGteChar :: String
dsGteChar = "gteChar"

-- | Boolean not.
dsNot :: String
dsNot = "not"

-- | Boolean and.
dsAnd :: String
dsAnd = "and"

-- | Boolean or.
dsOr :: String
dsOr = "or"

-- | If-then-else.
dsIfThenElse :: String
dsIfThenElse = "ifThenElse"

-- | Empty list literal.
dsNil :: String
dsNil = "nil"

-- | List cons operation.
dsCons :: String
dsCons = "cons"
