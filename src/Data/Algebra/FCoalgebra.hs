{-# LANGUAGE TypeFamilies #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- The 'FCoalgebra' class provides a general interface for all coinductive
-- types. An instance of the 'FCoalgebra' is defined by the type of elements in
-- the algebra 'FCoelem' along with a deconstructor 'funcons'. For example one
-- might define lists as a F-coalgebra by:
--
-- > instance FCoalgebra [a] where
-- >   type FCoelem [a] = Maybe (a, [a])
-- >   funcons []       = Nothing
-- >   funcons (x : xs) = Just (x, xs)
--
-- From this we are able to define all common operations for destroying lists.
--
-- @since 0.1.0.0

--
-- @since 0.1.0.0

module Data.Algebra.FCoalgebra
  ( -- * Initial F-coalgebras
    FCoalgebra(..)
    -- * Coinductive Operations
  , uncons
  , null
  , head
  , tail
    -- ** Searching
  --, lookup
  --, notElem
    -- ** Subcollections
  , dropPrefix
  , dropWhile
  , stripPrefix
  ) where

-- Local Modules
import Data.Algebra.FCoalgebra.Internal

import Data.Maybe
import Prelude hiding
  (head, tail, null, lookup, notElem, dropWhile)

-- | Deconstruct a coinductive type @a@.
--
-- @since 0.1.0.0
uncons :: FCoalgebra a => a -> FCoelem a
uncons = funcons
{-# INLINE uncons #-}

-- | Test whether a coinductive type is empty.
--
-- @since 0.1.0.0
null :: (FCoalgebra a, FCoelem a ~ Maybe b) => a -> Bool
null xs = maybe True (const False) (uncons xs)
{-# INLINE null #-}

-- | Retrieves the first element from a coinductive type @a@.
--
-- @since 0.1.0.0
head :: (FCoalgebra a, FCoelem a ~ Maybe (b, a)) => a -> Maybe b
head = fmap fst . funcons
{-# INLINE head #-}

-- | Retrieves the tail from a coinductive type @a@.
--
-- @since 0.1.0.0
tail :: (FCoalgebra a, FCoelem a ~ Maybe (b, a)) => a -> Maybe a
tail = fmap snd . funcons
{-# INLINE tail #-}

-- | Drops the longest prefix of a collection satisfying the predicate @p@.
--
-- @since 0.1.0.0
dropWhile :: (FCoalgebra a, FCoelem a ~ Maybe (b, a)) => (b -> Bool) -> a -> a
dropWhile p ls = case uncons ls of
  Just (x, xs) | p x       -> dropWhile p xs
               | otherwise -> xs
  Nothing                  -> ls
{-# INLINE dropWhile #-}

-- | The 'stripPrefix' function drops the given prefix from a list. It returns
-- 'Nothing' if the list did not start with the prefix given, or 'Just' the list
-- after the prefix, if it does.
--
-- @since 0.1.0.0
stripPrefix
  :: (FCoalgebra a, FCoelem a ~ Maybe (b, a), Eq b)
  => a -> a -> Maybe a
stripPrefix prefix cs = case uncons prefix of
  Just (p, ps) -> case uncons prefix of
    Just (x, xs) | p == x    -> stripPrefix ps xs
                 | otherwise -> Nothing
    Nothing                  -> Nothing
  Nothing      -> Just cs
{-# INLINE stripPrefix #-}

-- | Drops the prefix from a collection or otherwise returning the original
-- collection if the sequence doesn't start with the given prefix.
--
-- @since 0.1.0.0
dropPrefix :: (FCoalgebra a, FCoelem a ~ Maybe (b, a), Eq b) => a -> a -> a
dropPrefix a b = fromMaybe b (stripPrefix a b)
{-# INLINE dropPrefix #-}
