{-# LANGUAGE TypeFamilies #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- F-algebras with constructors and destructors.
--
-- @since 0.1.0.0

module Data.Algebra.FBialgebra
  ( -- * Initial F-Bialgebras
    FBialgebra
  , WType
    -- * Modifying Operations
  , takeWhile
  , span
  , break
  , zip
  , zipWith
  ) where

-- Local Modules
import Data.Algebra.FAlgebra
import Data.Algebra.FCoalgebra
import Data.Algebra.FBialgebra.Internal

import Prelude hiding (break, span, takeWhile, zip, zipWith)

-- | Takes the longest prefix of a collection satisfying the predicate @p@.
--
-- @since 0.1.0.0
takeWhile :: WType a b => (b -> Bool) -> a -> a
takeWhile p ls = case uncons ls of
  Just (x, xs)
    | p x       -> cons x (takeWhile p xs)
    | otherwise -> xs
  Nothing       -> ls
{-# INLINE takeWhile #-}

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list.
--
-- @since 0.1.0.0
span :: (WType a b, Monoid a) => (b -> Bool) -> a -> (a, a)
span p ls = case uncons ls of
  Just (x, xs)
    | p x       -> let (ys, zs) = span p xs in (cons x ys, zs)
    | otherwise -> (mempty, xs)
  Nothing       -> (ls, ls)
{-# INLINE span #-}

-- | The negation of 'span', equivalent to:
--
-- @span (not . p)@
--
-- @since 0.1.0.0
break :: (WType a b, Monoid a) => (b -> Bool) -> a -> (a, a)
break p = span (not . p)
{-# INLINE break #-}

-- | Takes two collections producing a new collection whose pointwise pairing of
-- the collections provided. If one collection is larger than the other the
-- excess elements are discarded.
--
-- @since 0.1.0.0
zip :: (WType a b, FAlgebra c, Monoid c, FElem c ~ ((b, b), c)) => a -> a -> c
zip = zipWith (,)
{-# INLINE zip #-}

-- | Takes two collections applying @f@ pointwise. If one collection is larger
-- than the other the excess elements are discarded.
--
-- @since 0.1.0.0
zipWith
  :: (WType a b, FAlgebra c, Monoid c, FElem c ~ (d, c))
  => (b -> b -> d) -> a -> a -> c
zipWith f cs1 cs2 = case uncons cs1 of
  Just (x, xs) -> case uncons cs2 of
    Just (y, ys) -> cons (f x y) (zipWith f xs ys)
    Nothing      -> mempty
  Nothing      -> mempty
{-# INLINE zipWith #-}
