{- |
Module      :  Data.Nullable
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines Nullable typeclass

It is currently used to use `unboxed` primitive types as "nullable" type
(i.e. the value of `Int` can be not set/unknown)

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Nullable
(

  Nullable(..)

) where

import Data.Bool
import Data.Maybe

-- | Some types may not have value set
-- Useful in cases when we cannot use `Maybe a` (i.e. for unboxed arrays in our case)
class Nullable a where
    null :: a
    isNull :: a -> Bool


instance Nullable (Maybe a) where
    {-# INLINE null #-}
    null = Nothing
    {-# INLINE isNull #-}
    isNull = isNothing
