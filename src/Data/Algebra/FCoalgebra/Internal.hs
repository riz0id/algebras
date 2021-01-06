{-# LANGUAGE TypeFamilies #-}

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

module Data.Algebra.FCoalgebra.Internal where

import           Data.Kind
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

-- | @since 0.1.0.0
instance FCoalgebra [a] where
  type FCoelem [a] = Maybe (a, [a])

  funcons []       = Nothing
  funcons (x : xs) = Just (x, xs)
  {-# INLINE funcons #-}

-- | @since 0.1.0.0
instance FCoalgebra (Seq a) where
  type FCoelem (Seq a) = Maybe (a, Seq a)

  funcons Seq.Empty      = Nothing
  funcons (x Seq.:<| xs) = Just (x, xs)
