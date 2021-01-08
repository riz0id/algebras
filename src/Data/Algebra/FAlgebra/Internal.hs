{-# LANGUAGE TypeFamilies #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- 'FAlgebra' is an initial F-algebra structure modelling all inductive types.
--
-- @since 0.1.0.0

module Data.Algebra.FAlgebra.Internal
  ( FAlgebra(..)
  ) where

import Data.List.NonEmpty as NonEmpty (NonEmpty(..))
import Data.Sequence      as Seq (Seq(..), (<|))
import Data.Text          as Text (Text, cons)

import Data.Kind

-- | Initial F-algebra in the category @a@.
--
-- @since 0.1.0.0
class FAlgebra a where
  -- | 'FElem' is the underlying endofunctor of @a@.
  --
  -- @since 0.1.0.0
  type FElem a :: Type

  -- | Morphism the F-algebra wields.
  --
  -- @since 0.1.0.0
  fcons :: FElem a -> a

-- | @since 0.1.0.0
instance FAlgebra [a] where
  type FElem [a] = (a, [a])

  fcons (x, xs) = x : xs
  {-# INLINE CONLIKE fcons #-}

-- | @since 0.1.0.0
instance FAlgebra (NonEmpty a) where
  type FElem (NonEmpty a) = (a, [a])

  fcons (x, xs) = x NonEmpty.:| xs
  {-# INLINE CONLIKE fcons #-}

-- | @since 0.1.0.0
instance FAlgebra (Seq a) where
  type FElem (Seq a) = (a, Seq a)

  fcons (x, xs) = x Seq.<| xs
  {-# INLINE CONLIKE fcons #-}

-- | @since 0.1.0.0
instance FAlgebra Text where
  type FElem Text = (Char, Text)

  fcons (x, xs) = Text.cons x xs
  {-# INLINE CONLIKE fcons #-}
