{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
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
-- @since 0.1.0.0

module Data.Algebra.FBialgebra.Internal
  ( -- * Initial F-bialgebras
    FBialgebra
  , WType
  ) where

import Data.Algebra.FAlgebra
import Data.Algebra.FCoalgebra

import Prelude hiding
  (take)

-- | A F-bialgebra is a inductive type which has a constructor and a destructor.
--
-- @since 0.1.0.0
class (FAlgebra a, FCoalgebra a) => FBialgebra a where
  -- | \(\mathcal{O}(n)\). Returns the prefix of @xs@ of length @n@. For the
  -- default case `n <= 0` then `take = id`. If `length xs <= n` we return an
  -- empty @xs@.
  --
  -- Note that if there exists a more efficent notion of drop for the specific
  -- data structure @a@ it should be provided here.
  --
  -- @since 0.1.0.0
  take :: Int -> a -> a

  default take :: (FCoelem a ~ Maybe (b, a), FElem a ~ (b, a)) => Int -> a -> a
  take n xs | n <= 0    = xs
            | otherwise = case funcons xs of
                Just (x, xs') -> fcons (x, (take (n - 1) xs'))
                Nothing       -> xs
  {-# INLINE take #-}

-- | These are the F-bialgebras which are most common and useful, fitting any
-- w-type who constructs from (x, xs) and destructs to Maybe (x, xs)
--
-- @since 0.1.0.0
type WType a b =
  ( FBialgebra a
  , FElem a ~ (b, a)
  , FCoelem a ~ Maybe (b, a)
  )
