{-# LANGUAGE TypeFamilies #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- The 'FAlgebra' class provides a general interface for all inductive types.
-- An instance of the 'FAlgebra' is defined by the type of elements in the
-- algebra 'FElem' along with a constructor 'fcons'. For example one might
-- define lists as a F-algebra by:
--
-- > instance FAlgebra [a] where
-- >   type FElem [a] = (x, xs)
-- >   fcons (x, xs) = x : xs
--
-- From 'FElem' and 'fcons' we are able to define all common operations for
-- building inductive types which are not provided by a 'Monoid' structure.
--
-- @since 0.1.0.0

module Data.Algebra.FAlgebra
  ( -- * Initial F-algebras
    FAlgebra(..)
    -- * Inductive Operations
  , cons
  , singleton
  , replicate
  ) where

-- Local Modules
import Data.Algebra.FAlgebra.Internal

import Prelude hiding (replicate)

-- | Append a element @b@ to a inductive type @a@.
--
-- @since 0.1.0.0
cons :: (FAlgebra a, FElem a ~ (b, a)) => b -> a -> a
cons = curry fcons
{-# INLINE cons #-}

-- | Create a singleton @a@ from the element @b@.
--
-- @since 0.1.0.0
singleton :: (FAlgebra a, FElem a ~ (b, a), Monoid a) => b -> a
singleton x = cons x mempty
{-# INLINE singleton #-}

-- | Builds of a collection of @n@ many elements @x@. If @n <= 0@ then 'mempty'
-- is returned.
--
-- @since 0.1.0.0
replicate :: (FAlgebra a, FElem a ~ (b, a), Monoid a) => Int -> b -> a
replicate n x | n <= 0    = mempty
              | otherwise = cons x (replicate (n - 1) x)
{-# INLINE replicate #-}
