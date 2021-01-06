{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

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

-- | A F-bialgebra is a inductive type which has a constructor and a destructor.
--
-- @since 0.1.0.0
type FBialgebra a = (FAlgebra a, FCoalgebra a)

-- | These are the F-bialgebras which are most common and useful, fitting any
-- w-type who constructs from (x, xs) and destructs to Maybe (x, xs)
--
-- @since 0.1.0.0
type WType a b =
  ( FBialgebra a
  , FElem a ~ (b, a)
  , FCoelem a ~ Maybe (b, a)
  )
