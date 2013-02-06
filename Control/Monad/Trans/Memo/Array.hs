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
It is therefore most suitable for the cases when the distribution of possible key values is within reasonable range and is rather dense (the best case: all values withing some range will be used). If this is the case then `MArray` gives both lookup update of O(1) and since it is in-place mutable array it is very fast. 

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances,
  RankNTypes, FunctionalDependencies #-}

module Control.Monad.Trans.Memo.Array
(

   -- * Generic ArrayCache
   ArrayCache(..),
   ArrayMemo(..),

   -- * ArrayCache for boxed types
   -- ** Using boxed ST array
   STArrayCache,
   runSTArrayMemoM,
   evalSTArrayMemoM,
   STArrayMemo(..),
   -- ** Using boxed IO array
   IOArrayCache,
   runIOArrayMemoM,
   evalIOArrayMemoM,

   -- * ArrayCache for unboxed types
   -- ** Using unboxed ST array
   STUArrayCache,
   runSTUArrayMemoM,
   evalSTUArrayMemoM,
   STUArrayMemo(..),
   -- ** Using unboxed IO array
   IOUArrayCache,
   runIOUArrayMemoM,
   evalIOUArrayMemoM,

) where


import Data.Function
import Data.Maybe (Maybe(..))
import Data.Array.ST
import Data.Array.IO
import Data.Array.Unboxed

import Data.Nullable
import Data.MaybeLike

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Fix
import System.IO

import Control.Monad.Memo.Class

-- | Memoization cache based on mutable arrays
newtype ArrayCache arr m a = ArrayCache { evalArrayCache :: arr -> m a }

instance (Functor m) => Functor (ArrayCache arr m) where
    {-# INLINE fmap #-}
    fmap f m = ArrayCache $ \arr -> fmap f (evalArrayCache m arr)

instance (Applicative m) => Applicative (ArrayCache arr m) where
    {-# INLINE pure #-}
    pure a   = ArrayCache $ \_ -> pure a
    {-# INLINE (<*>) #-}
    f <*> v = ArrayCache $ \ r -> evalArrayCache f r <*> evalArrayCache v r

instance (Monad m) => Monad (ArrayCache arr m) where
    {-# INLINE return #-}
    return a = ArrayCache $ \_ -> return a
    {-# INLINE (>>=) #-}
    m >>= k  = ArrayCache $ \arr -> do
        a <- evalArrayCache m arr
        evalArrayCache (k a) arr
    {-# INLINE (>>) #-}
    m >> k   = m >>= \ _ -> k

instance (MonadFix m) => MonadFix (ArrayCache arr m) where
    mfix f = ArrayCache $ \ arr -> mfix $ \ a -> evalArrayCache (f a) arr


instance (Ix k, MArray arr e m, MaybeLike e v) =>
    MonadCache k v (ArrayCache (arr k e) m) where
        {-# INLINE lookup #-}
        lookup k = ArrayCache $ \arr -> do
                     mv <- readArray arr k
                     return $ if isNothing mv then Nothing else Just (fromJust mv)
        {-# INLINE add #-}
        add k v = ArrayCache $ \arr -> writeArray arr k (just v)


instance (Ix k, MArray arr e m, MaybeLike e v) =>
    MonadMemo k v (ArrayCache (arr k e) m) where
        {-# INLINE memo #-}
        memo f k = ArrayCache $ \arr -> do
          mv <- readArray arr k
          if isNothing mv
             then do
               v <- evalArrayCache (f k) arr
               writeArray arr k (just v)
               return v
             else return (fromJust mv)

-- | Generic interface for running memo-function with `ArrayCache` 
class MonadMemo k v (ArrayCache (arr k e) m) =>
    ArrayMemo k v arr e m | v arr -> e, arr -> m where
       evalArrayMemoM :: ArrayCache (arr k e) m a -> (k,k) -> m a
       runArrayMemoM :: ArrayCache (arr k e) m a -> (k,k) -> m (a, arr k e)



-- ArrayCache for boxed types
-- --------------------------

-- | ArrayCache based on `STArray`
type STArrayCache s k e = ArrayCache (STArray s k e) (ST s)

-- | Computes result using boxed array `STArray` within `ST` monad
evalSTArrayMemoM :: ArrayMemo k v (STArray s) e (ST s) =>
                    STArrayCache s k e a -> (k,k) -> ST s a
evalSTArrayMemoM = evalArrayMemoM

-- | Computes result and the final cache using boxed array `STArray` within `ST` monad
runSTArrayMemoM :: ArrayMemo k v (STArray s) e (ST s) =>
                   STArrayCache s k e a -> (k,k) -> ST s (a, STArray s k e)
runSTArrayMemoM = runArrayMemoM

-- | Interface for evaluating memo-functions using boxed `STArray`
class STArrayMemo k v e | v -> e, v -> v where
    evalSTArrayMemo :: (forall s. ArrayMemo k v (STArray s) e (ST s) =>
                        STArrayCache s k e a) -> (k,k) -> a
    runSTArrayMemo  :: (forall s. ArrayMemo k v (STArray s) e (ST s) =>
                        STArrayCache s k e a) -> (k,k) -> (a, Array k e)


-- | ArrayCache based on `IOArray`
type IOArrayCache k v e = ArrayCache (IOArray k e) IO

-- | Computes result using boxed array `STArray` within `IO` monad
evalIOArrayMemoM :: ArrayMemo k v IOArray e IO =>
                    IOArrayCache k v e a -> (k,k) -> IO a
evalIOArrayMemoM = evalArrayMemoM

-- | Computes result and the final cache using boxed array `STArray` within `IO` monad
runIOArrayMemoM ::  ArrayMemo k v IOArray e IO =>
                    IOArrayCache k v e a -> (k,k) -> IO (a, IOArray k e)
runIOArrayMemoM = runArrayMemoM


-- ArrayCache for unboxed types
-- ----------------------------

-- | `ArrayCache` based on `STUArray`
type STUArrayCache s k e = ArrayCache (STUArray s k e) (ST s)

-- | Computes result using unboxed array `STUArray` within `ST` monad
evalSTUArrayMemoM :: ArrayMemo k v (STUArray s) e (ST s) =>
                     STUArrayCache s k e a -> (k,k) -> ST s a
evalSTUArrayMemoM = evalArrayMemoM

-- | Computes result and the final cache using unboxed `STUArray` within `ST` monad
runSTUArrayMemoM :: ArrayMemo k v (STUArray s) e (ST s) =>
                    STUArrayCache s k e a -> (k,k) -> ST s (a, STUArray s k e)
runSTUArrayMemoM = runArrayMemoM


-- | `ArrayCache` based on `IOUArray`
type IOUArrayCache k e = ArrayCache (IOUArray k e) IO

-- | Computes result using unboxed `STUArray` within `IO` monad
evalIOUArrayMemoM :: ArrayMemo k v IOUArray e IO =>
                     IOUArrayCache k e a -> (k,k) -> IO a
evalIOUArrayMemoM = evalArrayMemoM

-- | Computes result and the final cache using unboxed `STUArray` within `IO` monad
runIOUArrayMemoM :: ArrayMemo k v IOUArray e IO =>
                    IOUArrayCache k e a -> (k,k) -> IO (a, IOUArray k e)
runIOUArrayMemoM = runArrayMemoM


-- | Interface for evaluating memo-functions using unboxed `STUArray`
class STUArrayMemo k v e | v -> e, e -> v where
    evalSTUArrayMemo :: (forall s. ArrayMemo k v (STUArray s) e (ST s) =>
                         STUArrayCache s k e a) -> (k,k) -> a
    runSTUArrayMemo  :: (forall s. ArrayMemo k v (STUArray s) e (ST s) =>
                         STUArrayCache s k e a) -> (k,k) -> (a, UArray k e)