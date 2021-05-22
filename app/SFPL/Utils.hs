{-# LANGUAGE ConstraintKinds, PatternSynonyms #-}

-- | Utilities.
module SFPL.Utils
  ( -- * Pattern synonyms
    pattern (:>),
    
    -- * Contraint synonyms
    WithCS,
    
    -- * Helper functions
    (<$$>),
    addCols,
    arr,
    assocArr,
    bindM2,
    devError,
    hjoin,
    par,
    pair,
    vjoin,
  )
  where

import Data.Array.IArray hiding (Ix)
import qualified Data.Array.IArray as Arr (Ix)
import GHC.Stack
import SFPL.Base
import Text.Megaparsec.Pos
import Text.PrettyPrint

------------------------------------------------------------
-- Operator fixity

infixl 5 :>
infixl 4 <$$>

------------------------------------------------------------
-- Pattern synonyms

-- | A snoc-style pattern synonym for the list (:) constructor.
--
-- @since 1.0.0
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x = x : xs

------------------------------------------------------------
-- Constraint synonyms

-- | Add a 'HasCallStack' to a constraint.
--
-- @since 1.0.0
type WithCS c = (HasCallStack, c)

------------------------------------------------------------
-- Helper functions

-- | Infix synonym for 'mapM'.
(<$$>) :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
(<$$>) = mapM

-- | Add the specified amount of columns to a 'SourcePos'.
--
-- @since 1.0.0
addCols :: SourcePos -> Int -> SourcePos
addCols (SourcePos n l c) x = SourcePos n l (mkPos $ unPos c + x)

-- | Create an array from a list.
--
-- @since 1.0.0
arr :: (IArray a e, Arr.Ix i, Num i) => [e] -> a i e
arr elems = listArray (0, fromIntegral $ length elems - 1) elems

-- | Create an array from a list of associations.
--
-- @since 1.0.0
assocArr :: (IArray a e, Arr.Ix i, Num i) => [(i, e)] -> a i e
assocArr assocs = array (0, fromIntegral $ length assocs - 1) assocs

-- | Bind two values in a monad.
--
-- @since 1.0.0
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
  a <- ma
  b <- mb
  f a b

-- | Signal a development error. This function should be used whenever we would like to
-- assert that a given subexpression would never be evaluated under the assumption
-- that the system works. Evaluating this expression therefore signals a development error.
devError :: HasCallStack => String -> a
devError = error

-- | Join a list of documents using the given separator.
--
-- @since 1.0.0
hjoin :: String -> [Doc] -> Doc
hjoin sep = hcat . punctuate (text sep)

-- | Surround a document with parentheses if the context precedence is higher
-- than the given precedence. The first parameter is the context precedence,
-- the second is the current precedence.
--
-- @since 1.0.0
par :: Prec -> Prec -> Doc -> Doc
par p p' doc | p > p'    = parens doc
             | otherwise = doc

-- | Pair two values in an applicative context.
--
-- @since 1.0.0
pair :: Applicative f => f a -> f b -> f (a, b)
pair fa fb = (,) <$> fa <*> fb

-- | Join a list of documents using the given separator, starting each document on a new line.
--
-- @since 1.0.0
vjoin :: String -> [Doc] -> Doc
vjoin sep = vcat . punctuate (text sep)
