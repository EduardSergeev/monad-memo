{- |
Module      :  Data.MaybeLike.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines default instances of `MaybeLike` for most primitive "Unboxed" types

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleContexts #-}


module Data.MaybeLike.Instances
(

  module Data.Nullable,
  module Data.MaybeLike

) where

import Prelude (Float, Double)
import Data.Bool
import Data.Char
import Data.Int
import Data.Word

import Data.MaybeLike
import Data.Nullable


instance Nullable Char => MaybeLike Char Char where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Int => MaybeLike Int Int where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Int8 => MaybeLike Int8 Int8 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Int16 => MaybeLike Int16 Int16 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Int32 => MaybeLike Int32 Int32 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Int64 => MaybeLike Int64 Int64 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 


instance Nullable Word => MaybeLike Word Word where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Word8 => MaybeLike Word8 Word8 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Word16 => MaybeLike Word16 Word16 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Word32 => MaybeLike Word32 Word32 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Word64 => MaybeLike Word64 Word64 where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v


instance Nullable Float => MaybeLike Float Float where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance Nullable Double => MaybeLike Double Double where
    {-# INLINE nothing #-}
    nothing = null
    {-# INLINE isNothing #-}
    isNothing = isNull
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v