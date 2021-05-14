{-# LANGUAGE DeriveTraversable, OverloadedLists, PatternSynonyms, TypeFamilies, ViewPatterns #-}

-- | Length-aware list type.
module SFPL.Utils.List
  ( -- * Data types
    List (Nil, (:-)),
    
    -- List functions
    map,
    filter,
    replicate,
    uncons,
    
    -- * Conversions
    fromBasic,
    toBasic,
  )
  where

import Prelude hiding (map, filter, replicate)
import qualified Prelude as P

import Data.List (genericLength)
import Data.Foldable (Foldable (..))
import qualified GHC.Exts as E (IsList (..))

------------------------------------------------------------
-- Operator fixity

infixr 5 :-

------------------------------------------------------------
-- Data types

-- | The standard list type, except that it knows about its length.
-- This can be used to query the length of the list in @O(1)@ time.
--
-- Note that this type stores its length strictly. Therefore it cannot be used
-- to store infinite lists.
--
-- @since 1.0.0
data List a
  = -- | @'List' n xs@ is a list of length @n@ containing the elements of @xs@.
    -- Invariant: @n == length xs@.
    List !Integer [a]
  deriving
    ( Eq          -- ^ @since 1.0.0
    , Functor     -- ^ @since 1.0.0
    , Traversable -- ^ @since 1.0.0
    )

-- | The empty list.
--
-- @since 1.0.0
pattern Nil :: List a
pattern Nil <- List _ [] where
  Nil = List 0 []

-- | Cons operation.
--
-- @since 1.0.0
pattern (:-) :: a -> List a -> List a
pattern x :- xs <- (uncons -> Just (x, xs)) where
  x :- List n xs = List (n + 1) (x : xs)

instance Ord a => Ord (List a) where
  List _ xs `compare` List _ ys = xs `compare` ys

instance Show a => Show (List a) where
  show (List _ xs) = show xs

instance Foldable List where
  toList = toBasic
  length (List n _) = fromInteger n
  
  fold       = fold . toList
  foldMap f  = foldMap f . toList
  foldMap' f = foldMap' f . toList
  foldr f z  = foldr f z . toList
  foldr' f z = foldr' f z . toList
  foldl f z  = foldl f z . toList
  foldl' f z = foldl' f z . toList
  foldr1 f   = foldr1 f . toList
  foldl1 f   = foldl1 f . toList
  null       = null . toList
  elem x     = elem x . toList
  maximum    = maximum . toList
  minimum    = minimum . toList
  sum        = sum . toList
  product    = product . toList

instance Semigroup (List a) where
  List n xs <> List m ys = List (n + m) (xs ++ ys)

instance Monoid (List a) where
  mempty = Nil

instance E.IsList (List a) where
  type Item (List a) = a
  
  fromList = fromBasic
  toList = toList
  fromListN n = List (toInteger n)

------------------------------------------------------------
-- List functions

map :: (a -> b) -> List a -> List b
map = fmap

filter :: (a -> Bool) -> List a -> List a
filter f Nil = Nil
filter f (x :- xs)
  | f x       = x :- filter f xs
  | otherwise = filter f xs

replicate :: Integral b => b -> a -> List a
replicate n x = List (toInteger n) (P.replicate (fromIntegral n) x)

-- | Correctly decompose a list into head and tail. Returns 'Nothing' if the list is empty.
--
-- @since 1.0.0
uncons :: List a -> Maybe (a, List a)
uncons (List _ [])       = Nothing
uncons (List n (x : xs)) = Just (x, List (n - 1) xs)

------------------------------------------------------------
-- Helper functions

-- | Convert an ordinary list to a length-aware list.
-- Note that this function computes the length of the input strictly,
-- so it will be slow for very long lists (and will not terminate for infinite lists).
--
-- @since 1.0.0
fromBasic :: [a] -> List a
fromBasic xs = List (genericLength xs) xs

-- | Convert a length-aware list to an ordinary list.
-- This operation is O(1).
--
-- @since 1.0.0
toBasic :: List a -> [a]
toBasic (List _ xs) = xs
