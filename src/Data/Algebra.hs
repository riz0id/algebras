{-# LANGUAGE TypeFamilies #-}

-- | Module    :  Data.Algebra
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- F-Coalgebra typeclass and internal instances.
--
-- @since 0.1.0.0

module Data.Algebra
  ( -- * Algebras
    FAlgebra(..)
  , FCoalgebra(..)
  , FBialgebra
  , WType
    -- ** Constructing
  , cons
  , singleton
  , replicate
    -- ** Destructing
  , uncons
  , head
  , tail
  , dropPrefix
  , dropWhile
  , stripPrefix
    -- ** Modifying
  , takeWhile
  , span
  , break
  , zip
  , zipWith
    -- ** Conversion
  , transfer
    -- ** Comparison
  , null
  ) where

import Data.Algebra.FAlgebra
import Data.Algebra.FCoalgebra
import Data.Algebra.FBialgebra

import Prelude hiding
  (break, dropWhile, head, null, replicate, span, tail, takeWhile, zip, zipWith)

-- | 'transfer', is a natural isomorphism between two algebras. The constraints,
-- while indimidating, only say that if we know how to destruct @a@ and
-- construct @b@ we can freely convert an @a@ to a @b@.
--
-- @since 0.1.0.0
transfer
  :: ( FCoalgebra a
     , FAlgebra b
     , Monoid b
     , FCoelem a ~ Maybe (c, a)
     , FElem b ~ (c, b)
     )
  => a -> b
transfer ls = case uncons ls of
  Just (x, xs) -> cons x (transfer xs)
  Nothing      -> mempty
