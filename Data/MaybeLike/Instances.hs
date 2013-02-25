{- |
Module      :  Data.MaybeLike.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines default instances of `MaybeLike` for most primitive "Unboxed" types

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleContexts, FlexibleInstances #-}


module Data.MaybeLike.Instances
(

  module Data.MaybeLike

) where

import Data.Eq ((==))
import Prelude (Bounded(maxBound), (/), isNaN)
import Prelude (Float, Double)
import Data.Char
import Data.Int
import Data.Word
import qualified Data.Maybe as M

import Data.MaybeLike


instance MaybeLike (M.Maybe a) a where
    {-# INLINE nothing #-}
    nothing = M.Nothing
    {-# INLINE isNothing #-}
    isNothing = M.isNothing
    {-# INLINE just #-}
    just = M.Just
    {-# INLINE fromJust #-}
    fromJust = M.fromJust

instance MaybeLike Char Char where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Int Int where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Int8 Int8 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Int16 Int16 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Int32 Int32 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Int64 Int64 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 


instance MaybeLike Word Word where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Word8 Word8 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Word16 Word16 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Word32 Word32 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Word64 Word64 where
    {-# INLINE nothing #-}
    nothing = maxBound
    {-# INLINE isNothing #-}
    isNothing v = v == maxBound
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v


instance MaybeLike Float Float where
    {-# INLINE nothing #-}
    nothing = 0/0
    {-# INLINE isNothing #-}
    isNothing = isNaN
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v 

instance MaybeLike Double Double where
    {-# INLINE nothing #-}
    nothing = 0/0
    {-# INLINE isNothing #-}
    isNothing = isNaN
    {-# INLINE just #-}
    just v = v
    {-# INLINE fromJust #-}
    fromJust v = v