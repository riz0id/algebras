{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE TypeFamilies            #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- 'FCoalgebra' is an initial F-coalgebra modelling all coinductive types.
--
-- @since 0.1.0.0

module Data.Algebra.FCoalgebra.Internal
  ( FCoalgebra(..)
  ) where

import Data.Sequence      as Seq (Seq(..))
import Data.Text          as Text (Text, uncons)

import Data.Kind
import Prelude            hiding (take)

-- | Initial F-coalgebra in the category @a@.
--
-- @since 0.1.0.0
class FCoalgebra a where
  -- | 'FCoelem' is the underlying endofuctor of @a@.
  --
  -- @since 0.1.0.0
  type FCoelem a :: Type

  -- | Morphism wielded by the F-coalgebra.
  --
  -- @since 0.1.0.0
  funcons :: a -> FCoelem a

  -- | \(\mathcal{O}(n)\). Returns the suffix of @xs@ after the first @n@
  -- elements. For the default cause if `n <= 0` then `drop = id`. If
  -- `length xs <= n` we return an empty @xs@.
  --
  -- Note that if there exists a more efficent implementation (less than
  -- \(\mathcal{O}(n)\)) of drop for the specific datastructure @a@ it sohuld be
  -- provided here.
  --
  -- @since 0.1.0.0
  drop :: Int -> a -> a

  default drop
    :: (FCoelem a ~ Maybe (b, a)) => Int -> a -> a
  drop n xs | n <= 0    = xs
            | otherwise = case funcons xs of
                Just (_, xs') -> xs'
                Nothing       -> xs
  {-# INLINE drop #-}


-- | @since 0.1.0.0
instance FCoalgebra [a] where
  type FCoelem [a] = Maybe (a, [a])

  funcons []       = Nothing
  funcons (x : xs) = Just (x, xs)
  {-# INLINE CONLIKE funcons #-}

-- | @since 0.1.0.0
instance FCoalgebra (Seq a) where
  type FCoelem (Seq a) = Maybe (a, Seq a)

  funcons Seq.Empty      = Nothing
  funcons (x Seq.:<| xs) = Just (x, xs)
  {-# INLINE CONLIKE funcons #-}

-- | @since 0.1.0.0
instance FCoalgebra Text where
  type FCoelem Text = Maybe (Char, Text)

  funcons = Text.uncons
  {-# INLINE CONLIKE funcons #-}
