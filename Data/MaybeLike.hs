{- |
Module      :  Data.MaybeLike
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines MaybeLike typeclass - a generic way to look at some types as if they were Maybe

It is currently used to add maybe-ness to `unboxed` primitive types
in cases when it isn't possuble to just use `Maybe a` (e.g. unboxed arrays) 

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleInstances, FunctionalDependencies #-}

module Data.MaybeLike
(

  MaybeLike(..)

) where

import Data.Bool
import qualified Data.Maybe as M

-- | An abstract interface to a type which may not have any value
class MaybeLike a v | a -> v where
    nothing :: a
    isNothing :: a -> Bool
    just :: v -> a
    fromJust :: a -> v

-- | Default definition is for Maybe
instance MaybeLike (M.Maybe a) a where
    {-# INLINE nothing #-}
    nothing = M.Nothing
    {-# INLINE isNothing #-}
    isNothing = M.isNothing
    {-# INLINE just #-}
    just = M.Just
    {-# INLINE fromJust #-}
    fromJust = M.fromJust