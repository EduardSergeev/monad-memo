{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

VectorCache - mutable-vector-based (`IO` and `ST` hosted) `MonadCache`

The fastest memoization cache, however it is even more limiting than "Control.Monad.Memo.Mutable.Array" due to nature of "Data.Vector.Mutable". Still if you can use this cache please do since it will give you dramatic calculation speed up in comparison to pure `Data.Map.Map`-based cache, especially when unboxed `UVectorCache` is used.

Limitations: Since `Data.Vector.Generic.Mutable.MVector` is used as `MonadCache` the key must be `Int` and the size of the cache's vector must be known beforehand with vector being allocated before the first call. In addition unboxed `UVectorCache` can only store `Data.Vector.Unboxed.Unbox` values (but it does it very efficiently).

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module Control.Monad.Memo.Mutable.Vector
 (

   -- * VectorCache for boxed types
   Vector,
   VectorCache,
   VectorMemo(..),
   -- * UVectorCache for unboxed types
   UVector,
   UVectorCache,
   UVectorMemo(..)

) where 

import Data.Int
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad
import Control.Monad.Primitive

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Memo.Mutable


-- VectorCache for boxed types
-- --------------------------
type Vector = M.MVector

type VectorCache m e = MutableCache (Vector (PrimState m) e) m

class (PrimMonad m, MaybeLike e v) => VectorMemo v e m | v -> e where
    evalVectorMemo :: VectorCache m e a -> Int -> m a
    {-# INLINE evalVectorMemo #-}
    evalVectorMemo m n = do
      vec <- M.replicate n nothing
      evalMutableCache m vec
    runVectorMemo :: VectorCache m e a -> Int -> m (a, Vector (PrimState m) e)
    {-# INLINE runVectorMemo #-}
    runVectorMemo m n = do
      vec <- M.replicate n nothing
      a <- evalMutableCache m vec
      return (a, vec)

instance (PrimMonad m, PrimState m ~ s) => Mutable Int e (Vector s e) m where
    {-# INLINE mutableRead #-}
    mutableRead = M.read
    {-# INLINE mutableWrite #-}
    mutableWrite = M.unsafeWrite


-- VectorCache for unboxed types
-- ----------------------------
type UVector = UM.MVector

type UVectorCache m e = MutableCache (UVector (PrimState m) e) m

class (PrimMonad m, MaybeLike e v, Unbox e) => UVectorMemo v e m | v -> e where
    evalUVectorMemo :: UVectorCache m e a -> Int -> m a
    {-# INLINE evalUVectorMemo #-}
    evalUVectorMemo m n = do
      vec <- UM.replicate n nothing
      evalMutableCache m vec
    runUVectorMemo :: UVectorCache m e a -> Int -> m (a, UVector (PrimState m) e)
    {-# INLINE runUVectorMemo #-}
    runUVectorMemo m n = do
      vec <- UM.replicate n nothing
      a <- evalMutableCache m vec
      return (a, vec)

instance (PrimMonad m, PrimState m ~ s, Unbox e) => Mutable Int e (UVector s e) m where
    {-# INLINE mutableRead #-}
    mutableRead = UM.read
    {-# INLINE mutableWrite #-}
    mutableWrite = UM.unsafeWrite
