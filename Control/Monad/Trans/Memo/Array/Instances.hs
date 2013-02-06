{- |
Module      :  Control.Monad.Trans.Memo.Array.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Default instances of `ArrayMemo`, `STArrayMemo` and `STUArrayMemo`

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, UndecidableInstances,
  FlexibleContexts, FlexibleInstances #-}


module Control.Monad.Trans.Memo.Array.Instances
(
  
  module Control.Monad.Trans.Memo.Array,
  module Data.Nullable,
  module Data.MaybeLike

) where

import Prelude (Float, Double)
import Data.Function
import Data.Bool
import Data.Char
import Data.Int
import Data.Word
import Data.Maybe

import Data.Array.ST
import Data.Array.IO
import Data.Array.Unboxed

import Data.Nullable
import Data.MaybeLike

import System.IO
import Control.Monad
import Control.Monad.ST

import Control.Monad.Trans.Memo.Array



instance (Ix k, MaybeLike (Maybe a) a, Nullable (Maybe a)) =>
    ArrayMemo k a IOArray (Maybe a) IO where
        {-# INLINE evalArrayMemoM #-}
        evalArrayMemoM = evalArrayMemoMImpl
        {-# INLINE runArrayMemoM #-}
        runArrayMemoM = runArrayMemoMImpl

instance (Ix k, MaybeLike (Maybe a) a, Nullable (Maybe a)) =>
    ArrayMemo k a (STArray s) (Maybe a) (ST s) where
        {-# INLINE evalArrayMemoM #-}
        evalArrayMemoM = evalArrayMemoMImpl
        {-# INLINE runArrayMemoM #-}
        runArrayMemoM = runArrayMemoMImpl

instance (Ix k, MaybeLike v v, Nullable v, MArray IOUArray v IO) =>
    ArrayMemo k v IOUArray v IO where
        {-# INLINE evalArrayMemoM #-}
        evalArrayMemoM = evalArrayMemoMImpl
        {-# INLINE runArrayMemoM #-}
        runArrayMemoM = runArrayMemoMImpl

instance (Ix k, MaybeLike v v, Nullable v, MArray (STUArray s) v (ST s)) =>
    ArrayMemo k v (STUArray s) v (ST s) where
        {-# INLINE evalArrayMemoM #-}
        evalArrayMemoM = evalArrayMemoMImpl
        {-# INLINE runArrayMemoM #-}
        runArrayMemoM = runArrayMemoMImpl

evalArrayMemoMImpl m lu = do
  arr <- newArray lu null
  evalArrayCache m arr

runArrayMemoMImpl m lu = do
  arr <- newArray lu null
  a <- evalArrayCache m arr
  return (a, arr)


instance Ix k => STArrayMemo k v (Maybe v) where
    {-# INLINE evalSTArrayMemo #-}
    evalSTArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTArrayMemo #-}
    runSTArrayMemo m lu = runST $ runSTMemoImpl m lu


instance (Ix k, MaybeLike Char Char, Nullable Char) => STUArrayMemo k Char Char where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Int Int, Nullable Int) => STUArrayMemo k Int Int where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Int8 Int8, Nullable Int8) => STUArrayMemo k Int8 Int8 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Int16 Int16, Nullable Int16) => STUArrayMemo k Int16 Int16 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Int32 Int32, Nullable Int32) => STUArrayMemo k Int32 Int32 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Int64 Int64, Nullable Int64) => STUArrayMemo k Int64 Int64 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Word Word, Nullable Word) => STUArrayMemo k Word Word where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Word8 Word8, Nullable Word8) => STUArrayMemo k Word8 Word8 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Word16 Word16, Nullable Word16) => STUArrayMemo k Word16 Word16 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Word32 Word32, Nullable Word32) => STUArrayMemo k Word32 Word32 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Word64 Word64, Nullable Word64) => STUArrayMemo k Word64 Word64 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Float Float, Nullable Float) => STUArrayMemo k Float Float where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

instance (Ix k, MaybeLike Double Double, Nullable Double) => STUArrayMemo k Double Double where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runSTMemoImpl m lu

runSTMemoImpl m lu = do
      (a, arr) <- runArrayMemoM m lu
      iarr <- freeze arr
      return (a, iarr)
