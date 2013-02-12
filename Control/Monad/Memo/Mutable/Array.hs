{- |
Module      :  Control.Monad.Trans.Memo.Array
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

ArrayCache - mutable-array-based (`IO` and `ST` hosted) `MonadCache`

Very fast memoization cache. Unfortunatelly it cannot suit every case (see limitations), but if you can use it, please do: it is generally an order of magnitude faster than `Data.Map`-based `Control.Monad.Trans.Memo.Map.MemoT`, especially /unboxed/ version - try to use it whenever you can.

Limitations: Since `MArray` is used as `MonadCache` the key range must be known beforehand and the array is allocated before the first call.
It is therefore most suitable for the cases when the distribution of possible key values is within reasonable range and is rather dense (the best case: all values withing some range will be used). If this is the case then `MArray` has O(1) for both lookup and update operations.
In addition unboxed `UArrayCache` can only store unboxed types (but it does it very efficiently).

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts,
  UndecidableInstances, TypeFamilies #-}

module Control.Monad.Memo.Mutable.Array
(

   -- * ArrayCache for boxed types
   Array(..),
   ArrayCache,
   ArrayMemo(..),
   -- * ArrayCache for unboxed types
   UArray,
   UArrayCache,
   UArrayMemo(..),

) where


import Data.Array.ST
import Data.Array.IO
import Control.Monad
import Control.Monad.ST
import System.IO

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Memo.Mutable


-- ArrayCache for boxed types
-- --------------------------
type family Array (m :: * -> *) :: * -> * -> *

type instance Array (ST s) = STArray s
type instance Array IO = IOArray
type ArrayCache m k e = MutableCache (Array m k e) m

class (Monad m, Ix k, MArray (Array m) e m, MaybeLike e v) => ArrayMemo k v e m | v -> e where

    evalArrayMemo :: ArrayCache m k e a -> (k,k) -> m a
    {-# INLINE evalArrayMemo #-}
    evalArrayMemo m lu = do
      arr <- newArray lu nothing
      evalMutableCache m arr

    runArrayMemo :: ArrayCache m k e a -> (k,k) -> m (a, Array m k e)
    {-# INLINE runArrayMemo #-}
    runArrayMemo m lu = do
      arr <- newArray lu nothing
      a <- evalMutableCache m arr
      return (a, arr)

instance Ix k => Mutable k e (STArray s k e) (ST s) where
    {-# INLINE mutableRead #-}
    mutableRead = readArray
    {-# INLINE mutableWrite #-}
    mutableWrite = writeArray

instance Ix k => Mutable k e (IOArray k e) IO where
    {-# INLINE mutableRead #-}
    mutableRead = readArray
    {-# INLINE mutableWrite #-}
    mutableWrite = writeArray


-- ArrayCache for unboxed types
-- ----------------------------
type family UArray (m :: * -> *) :: * -> * -> *

type instance UArray (ST s) = STUArray s
type instance UArray IO = IOUArray

type UArrayCache m k e = MutableCache (UArray m k e) m

class (Monad m, Ix k, MArray (UArray m) e m, MaybeLike e v) => UArrayMemo k v e m | v -> e where
    evalUArrayMemo :: UArrayCache m k e a -> (k,k) -> m a
    {-# INLINE evalUArrayMemo #-}
    evalUArrayMemo m lu = do
      arr <- newArray lu nothing
      evalMutableCache m arr
    runUArrayMemo :: UArrayCache m k e a -> (k,k) -> m (a, UArray m k e)
    {-# INLINE runUArrayMemo #-}
    runUArrayMemo m lu = do
      arr <- newArray lu nothing
      a <- evalMutableCache m arr
      return (a, arr)

instance (Ix k, MArray (STUArray s) e (ST s)) => Mutable k e (STUArray s k e) (ST s) where
    {-# INLINE mutableRead #-}
    mutableRead = readArray
    {-# INLINE mutableWrite #-}
    mutableWrite = writeArray

instance (Ix k, MArray IOUArray e IO) => Mutable k e (IOUArray k e) IO where
    {-# INLINE mutableRead #-}
    mutableRead = readArray
    {-# INLINE mutableWrite #-}
    mutableWrite = writeArray