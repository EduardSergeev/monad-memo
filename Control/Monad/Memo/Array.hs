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

module Control.Monad.Memo.Array
(

   -- * ArrayCache for boxed types
   Array(..),
   ArrayCache,
   ArrayMemo,
   evalArrayMemo,
   runArrayMemo,
   -- * ArrayCache for unboxed types
   UArray,
   UArrayCache,
   UArrayMemo,
   evalUArrayMemo,
   runUArrayMemo

) where


import Data.Function
import Data.Maybe (Maybe(..))
import Data.Array.ST
import Data.Array.IO
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.ST
import System.IO

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.ReaderCache


newtype Cache c k e m a = Cache { toReaderCache :: ReaderCache (c k e) m a }

instance Functor m => Functor (Cache c k e m) where
    {-# INLINE fmap #-}
    fmap f m = Cache $ fmap f (toReaderCache m)

instance (Functor m, Applicative m) => Applicative (Cache c k e m) where
    {-# INLINE pure #-}
    pure = Cache . pure
    {-# INLINE (<*>) #-}
    a <*> b = Cache $ toReaderCache a <*> toReaderCache b

instance Monad m => Monad (Cache c k e m) where
    {-# INLINE return #-}
    return = Cache . return
    {-# INLINE (>>=) #-}
    m >>= f = Cache $ toReaderCache m >>= toReaderCache . f

instance MonadTrans (Cache c k e) where
    {-# INLINE lift #-}
    lift = Cache . lift

instance MonadFix m => MonadFix (Cache c k e m) where
    {-# INLINE mfix #-}
    mfix f = Cache $ mfix $ \a -> toReaderCache (f a)


instance (Monad m, Ix k, MaybeLike e v, MArray c e m) =>
    MonadCache k v (Cache c k e m) where
        {-# INLINE lookup #-}
        lookup k = do
          c <- Cache container
          e <- lift $ readArray c k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- Cache container
          lift $ writeArray c k (just v) 

instance (Monad m, Ix k, MaybeLike e v, MArray c e m) =>
    MonadMemo k v (Cache c k e m) where
        {-# INLINE memo #-}
        memo f k = do
          c <- Cache container
          e <- lift $ readArray c k
          if isNothing e
            then do
              v <- f k
              lift $ writeArray c k (just v)
              return v
            else return (fromJust e) 


-- ArrayCache for boxed types
-- --------------------------
type family Array (m :: * -> *) :: * -> * -> *

type instance Array (ST s) = STArray s
type instance Array IO = IOArray
type ArrayCache k e m = Cache (Array m) k e m

class MaybeLike e v => ArrayMemo v e | v -> e

evalArrayMemo :: (Ix k, MArray (Array m) e m, ArrayMemo v e) =>
                 ArrayCache k e m a -> (k,k) -> m a
{-# INLINE evalArrayMemo #-}
evalArrayMemo = genericEvalArrayMemo

runArrayMemo :: (Ix k, MArray (Array m) e m, ArrayMemo v e) =>
                ArrayCache k e m a -> (k,k) -> m (a, Array m k e)
{-# INLINE runArrayMemo #-}
runArrayMemo = genericRunArrayMemo


-- ArrayCache for unboxed types
-- ----------------------------
type family UArray (m :: * -> *) :: * -> * -> *

type instance UArray (ST s) = STUArray s
type instance UArray IO = IOUArray

type UArrayCache k e m = Cache (UArray m) k e m

class MaybeLike e v => UArrayMemo v e | v -> e

evalUArrayMemo :: (Ix k, MArray (UArray m) e m, UArrayMemo v e) =>
                  UArrayCache k e m a -> (k,k) -> m a
{-# INLINE evalUArrayMemo #-}
evalUArrayMemo = genericEvalArrayMemo

runUArrayMemo :: (Ix k, MArray (UArray m) e m, UArrayMemo v e) =>
                 UArrayCache k e m a -> (k,k) -> m (a, UArray m k e)
{-# INLINE runUArrayMemo #-}
runUArrayMemo = genericRunArrayMemo


{-# INLINE genericEvalArrayMemo #-}
genericEvalArrayMemo m lu = do
  c <- newArray lu nothing
  evalReaderCache (toReaderCache m) c

{-# INLINE genericRunArrayMemo #-}
genericRunArrayMemo m lu = do
  c <- newArray lu nothing
  a <- evalReaderCache (toReaderCache m) c
  return (a, c)
