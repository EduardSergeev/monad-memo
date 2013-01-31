{- |
Module      :  Control.Monad.Trans.Memo.Stat
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

ArrayCache - mutable-array-based (`IO` and `ST` hosted) `MonadCache`

The fastest kind of memoization cache. Unfortunatelly it cannot suit every case (see limitations), but if you can use it, please do: it is generally an order of magnitude faster than Map-based MemoCache, especially `Unboxed` version - try to use it if possible.

Limitations: Since `MArray` is used as `MonadCache` the key range must be known beforehand and the array is allocated before the first call.
It is therefore most suitable for the cases when the distribution of possible key values is within reasonable range and is rather dense (the best case: all values withing some range will be used). If this is the case then `MArray` gives both lookup update of O(1) and since it is in-place mutable array it is very fast. 

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, UndecidableInstances,
  RankNTypes, FunctionalDependencies #-}

module Control.Monad.Trans.Memo.Array
(

   ArrayCache,

   UArrayMemo(..),
   STUArrayMemo(..),

   runArrayMemoM,
   evalArrayMemoM,

   runSTArrayMemoM,
   evalSTArrayMemoM,
   runSTArrayMemo,
   evalSTArrayMemo,

   runSTUArrayMemoM,
   evalSTUArrayMemoM,

   runIOArrayMemoM,
   evalIOArrayMemoM,
   runIOUArrayMemoM,
   evalIOUArrayMemoM

) where


import Data.Function
import Data.Maybe (Maybe(..))
import Data.Array.ST
import Data.Array.IO
import Data.Array.IArray
import Data.Array.Unboxed

import Data.Nullable
import Data.MaybeLike

import Control.Applicative
import Control.Monad.ST
import Control.Monad.Reader
import System.IO

import Control.Monad.Memo.Class


newtype ArrayCache arr m a = ArrayCache { runArrayCache :: arr -> m a }

instance (Functor m) => Functor (ArrayCache arr m) where
    fmap f m = ArrayCache $ \arr -> fmap f ((runArrayCache m) arr)

instance (Applicative m) => Applicative (ArrayCache arr m) where
    {-# INLINE pure #-}
    pure a   = ArrayCache $ \_ -> pure a
    {-# INLINE (<*>) #-}
    f <*> v = ArrayCache $ \ r -> runArrayCache f r <*> runArrayCache v r

instance (Monad m) => Monad (ArrayCache arr m) where
    {-# INLINE return #-}
    return a = ArrayCache $ \_ -> return a
    {-# INLINE (>>=) #-}
    m >>= k  = ArrayCache $ \arr -> do
        a <- runArrayCache m arr
        runArrayCache (k a) arr
    {-# INLINE (>>) #-}
    m >> k   = m >>= \ _ -> k


instance (Monad m, Ix k, MArray arr mv m, MaybeLike mv v) =>
    MonadCache k v (ArrayCache (arr k mv) m) where
        {-# INLINE lookup #-}
        lookup k = ArrayCache $ \arr -> do
                     mv <- readArray arr k
                     return $ if isNothing mv then Nothing else Just (fromJust mv)
        {-# INLINE add #-}
        add k v = ArrayCache $ \arr -> writeArray arr k (just v)


instance (Monad m, Ix k, MArray arr mv m, MaybeLike mv v) =>
    MonadMemo k v (ArrayCache (arr k mv) m) where
        {-# INLINE memo #-}
        memo f k = ArrayCache $ \arr -> do
          mv <- readArray arr k
          if isNothing mv
             then do
               v <- runArrayCache (f k) arr
               writeArray arr k (just v)
               return v
             else return (fromJust mv)


{-# INLINE evalArrayMemoM #-}
evalArrayMemoM :: (Monad m, Ix k, Nullable mv, MArray arr mv m) =>
                 ArrayCache (arr k mv) m a -> (k,k) -> m a
evalArrayMemoM m lu = do
    arr <- newArray lu null
    runArrayCache m arr

{-# INLINE runArrayMemoM #-}
runArrayMemoM :: (Monad m, Ix k, Nullable av, MArray arr av m, IArray ia av) =>
                 ArrayCache (arr k av) m a -> (k,k) -> m (a, ia k av)
runArrayMemoM m lu = do
    arr <- newArray lu null
    a <- runArrayCache m arr
    iarr <- freeze arr
    return (a, iarr)


evalSTArrayMemoM :: Ix k => ArrayCache (STArray s k (Maybe v)) (ST s) a -> (k,k) ->
                    ST s a
evalSTArrayMemoM = evalArrayMemoM

runSTArrayMemoM :: Ix k => ArrayCache (STArray s k (Maybe v)) (ST s) a -> (k,k) ->
                   ST s (a, Array k (Maybe v))
runSTArrayMemoM = runArrayMemoM


evalSTArrayMemo :: (Ix k) => (forall s. ArrayCache (STArray s k (Maybe v)) (ST s) a) -> (k,k) ->
                   a
evalSTArrayMemo m lu = runST $ evalArrayMemoM m lu

runSTArrayMemo :: (Ix k) =>
                  (forall s. ArrayCache (STArray s k (Maybe v)) (ST s) a) -> (k,k) ->
                  (a, Array k (Maybe v))
runSTArrayMemo m lu = runST $ runArrayMemoM m lu


evalIOArrayMemoM :: (Ix k, MArray IOArray (Maybe v) IO) =>
                    ArrayCache (IOArray k (Maybe v)) IO a -> (k,k) -> IO a
evalIOArrayMemoM = evalArrayMemoM

runIOArrayMemoM :: (Ix k, MArray IOArray (Maybe v) IO) =>
                    ArrayCache (IOArray k (Maybe v)) IO a -> (k,k) -> IO (a, Array k (Maybe v))
runIOArrayMemoM = runArrayMemoM


class (Ix k, MaybeLike av v) => UArrayMemo k v arr av m | v -> av, m -> arr where
        evalUArrayMemoM :: ArrayCache (arr k av) m a -> (k,k) -> m a
        runUArrayMemoM :: ArrayCache (arr k av) m a -> (k,k) -> m (a, UArray k av)


evalSTUArrayMemoM :: (Ix k, Nullable av, MArray (STUArray s) av (ST s)) =>
                     ArrayCache (STUArray s k av) (ST s) a -> (k,k) -> ST s a
evalSTUArrayMemoM = evalArrayMemoM

runSTUArrayMemoM :: (Ix k, Nullable av, MArray (STUArray s) av (ST s), IArray UArray av) =>
                    ArrayCache (STUArray s k av) (ST s) a -> (k,k) ->
                    ST s (a, UArray k av)
runSTUArrayMemoM = runArrayMemoM


class STUArrayMemo v where
    evalSTUArrayMemo :: Ix k => (forall s. UArrayMemo k v (STUArray s) v (ST s) =>
                        ArrayCache (STUArray s k v) (ST s) a) -> (k,k) -> a
    runSTUArrayMemo  :: Ix k => (forall s. UArrayMemo k v (STUArray s) v (ST s) =>
                        ArrayCache (STUArray s k v) (ST s) a) -> (k,k) ->
                        (a, UArray k v)


evalIOUArrayMemoM :: (Ix k, Nullable av, MArray IOUArray av IO) =>
                     ArrayCache (IOUArray k av) IO a -> (k,k) -> IO a
evalIOUArrayMemoM = evalArrayMemoM

runIOUArrayMemoM :: (Ix k, Nullable av, MArray IOUArray av IO, IArray UArray av) =>
                    ArrayCache (IOUArray k av) IO a -> (k,k) ->
                    IO (a, UArray k av)
runIOUArrayMemoM = runArrayMemoM
