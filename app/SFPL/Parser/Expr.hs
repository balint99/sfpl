{-# LANGUAGE LambdaCase #-}

-- | General expression parser for unary prefix and binary infix operators,
-- support precise span parsing.
module SFPL.Parser.Expr
  ( Operator (..),
    makeExprParser,
  )
  where

import Control.Applicative ((<**>))
import Control.Monad.Combinators
import SFPL.Base
import SFPL.Syntax.Raw.Types
import Text.Megaparsec (MonadParsec, TraversableStream, getSourcePos)

withB :: (TraversableStream s, MonadParsec e s m) => m (BegPos -> a) -> m a
withB p = getSourcePos <**> p

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix. A binary operator also has
-- an associated associativity.
--
-- @since 1.0.0
data Operator m a
  = -- | Binary infix. The parser action returns a function which combines the
    -- results of parsing two operands.
    Infix Assoc (m (a -> a -> a))
  | -- | Unary prefix. The parser action returns a function which
    -- transforms the result of parsing the operand, and also accepts the
    -- beginning source position.
    Prefix (m (a -> BegPos -> a))

-- | Creates an expressions from the given operator table.
-- The operator table contains operators, ordered by decreasing precedence.
-- Operators in the same list have the same precedence.
--
-- @since 1.0.0
makeExprParser ::
  (TraversableStream s, MonadParsec e s m) => 
  -- | Term parser
  m (BegPos -> a) ->
  -- | Operator table
  [[Operator m a]] ->
  -- | Resulting expression parser
  m (BegPos -> a)
makeExprParser = foldl addPrecLevel
{-# INLINEABLE makeExprParser #-}

-- | @addPrecLevel p ops@ adds the ability to parse operators in table @ops@
-- to parser @p@.
addPrecLevel :: (TraversableStream s, MonadParsec e s m) =>
  m (BegPos -> a) -> [Operator m a] -> m (BegPos -> a)
addPrecLevel term ops = 
  term' >>= \t -> choice [rs' t, ls' t, ns' t, pure t]
  where
    (rs, ls, ns, pres) = foldr splitOp ([], [], [], []) ops
    term' = pTerm (choice pres) term
    rs' = pInfixR (choice rs) term'
    ls' = pInfixL (choice ls) term'
    ns' = pInfixN (choice ns) term'
{-# INLINEABLE addPrecLevel #-}

-- | @pTerm prefix term@ parses a @term@ preceded by an optional
-- prefix unary operator. Parser @prefix@ is allowed to fail.
pTerm :: (TraversableStream s, MonadParsec e s m) =>
  m (a -> BegPos -> a) -> m (BegPos -> a) -> m (BegPos -> a)
pTerm prefix term = do
  pre <- optional prefix
  case pre of
    Nothing -> term
    Just f  -> f <$> withB term
{-# INLINE pTerm #-}

-- | @pInfixR op p x@ parses right-associative infix operator @op@, then
-- term with parser @p@, then returns result of the operator application on
-- @x@ and the term.
pInfixR :: (TraversableStream s, MonadParsec e s m) =>
  m (a -> a -> a) -> m (BegPos -> a) -> (BegPos -> a) -> m (BegPos -> a)
pInfixR op p x = do
  f <- op
  y <- withB $ p >>= \r -> pInfixR op p r <|> pure r
  pure $ f <$> x <*> pure y
{-# INLINE pInfixR #-}

-- | @pInfixL op p x@ parses left-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.
pInfixL :: (TraversableStream s, MonadParsec e s m) =>
  m (a -> a -> a) -> m (BegPos -> a) -> (BegPos -> a) -> m (BegPos -> a)
pInfixL op p x = do
  f <- op
  y <- withB p
  let r = f <$> x <*> pure y
  pInfixL op p r <|> pure r
{-# INLINE pInfixL #-}

-- | @pInfixN op p x@ parses non-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.
pInfixN ::(TraversableStream s, MonadParsec e s m) =>
  m (a -> a -> a) -> m (BegPos -> a) -> (BegPos -> a) -> m (BegPos -> a)
pInfixN op p x = do
  f <- op
  y <- withB p
  pure $ f <$> x <*> pure y
{-# INLINE pInfixN #-}

type Batch m a =
  ( [m (a -> a -> a)],
    [m (a -> a -> a)],
    [m (a -> a -> a)],
    [m (a -> BegPos -> a)]
  )

-- | A helper to separate various operators (binary, unary, and according to
-- associativity) and return them in a tuple.
splitOp :: Operator m a -> Batch m a -> Batch m a
splitOp op (r, l, n, pre) = case op of
  Infix RightAssoc op -> (op : r, l, n, pre)
  Infix LeftAssoc  op -> (r, op : l, n, pre)
  Infix None       op -> (r, l, op : n, pre)
  Prefix           op -> (r, l, n, op : pre)
