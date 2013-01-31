{- |
Module      :  Data.Nullable.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines default `Nullable` instances for most primitive "Unboxed" types

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Nullable.Instances (

  module Data.Nullable

) where

import Prelude (Bounded(..),  RealFloat(..),
                Double, Float, (/))
import Data.Eq
import Data.Bool
import Data.Char
import Data.Int
import Data.Word

import Data.Nullable

instance Nullable Char where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound


instance Nullable Int where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Int8 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Int16 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Int32 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Int64 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound


instance Nullable Word where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Word8 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Word16 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Word32 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound

instance Nullable Word64 where
    {-# INLINE null #-}
    null = minBound
    {-# INLINE isNull #-}
    isNull a = a == minBound


instance Nullable Float where
    {-# INLINE null #-}
    null = 0/0
    {-# INLINE isNull #-}
    isNull a = isNaN a

instance Nullable Double where
    {-# INLINE null #-}
    null = 0/0
    {-# INLINE isNull #-}
    isNull a = isNaN a