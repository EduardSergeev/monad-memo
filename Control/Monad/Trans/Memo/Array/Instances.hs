{- |
Module      :  Control.Monad.Trans.Memo.Array.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines default instances of `UArrayMemo` and `STUArrayMemo`

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

import Data.Array.ST
import Data.Array.IO

import Data.Nullable
import Data.MaybeLike

import System.IO
import Control.Monad.ST

import Control.Monad.Trans.Memo.Array

instance (Ix k, MaybeLike Char Char, Nullable Char) =>
    UArrayMemo k Char IOUArray Char IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int Int, Nullable Int) =>
    UArrayMemo k Int IOUArray Int IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM
 
instance (Ix k, MaybeLike Int8 Int8, Nullable Int8) =>
    UArrayMemo k Int8 IOUArray Int8 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int16 Int16, Nullable Int16) =>
    UArrayMemo k Int16 IOUArray Int16 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int32 Int32, Nullable Int32) =>
    UArrayMemo k Int32 IOUArray Int32 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int64 Int64, Nullable Int64) =>
    UArrayMemo k Int64 IOUArray Int64 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word Word, Nullable Word) =>
    UArrayMemo k Word IOUArray Word IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word8 Word8, Nullable Word8) =>
    UArrayMemo k Word8 IOUArray Word8 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word16 Word16, Nullable Word16) =>
    UArrayMemo k Word16 IOUArray Word16 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word32 Word32, Nullable Word32) =>
    UArrayMemo k Word32 IOUArray Word32 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word64 Word64, Nullable Word64) =>
    UArrayMemo k Word64 IOUArray Word64 IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Float Float, Nullable Float) =>
    UArrayMemo k Float IOUArray Float IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Double Double, Nullable Double) =>
    UArrayMemo k Double IOUArray Double IO where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM


instance (Ix k, MaybeLike Char Char, Nullable Char) =>
    UArrayMemo k Char (STUArray s) Char (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM


instance (Ix k, MaybeLike Int Int, Nullable Int) =>
    UArrayMemo k Int (STUArray s) Int (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int8 Int8, Nullable Int8) =>
    UArrayMemo k Int8 (STUArray s) Int8 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int16 Int16, Nullable Int16) =>
    UArrayMemo k Int16 (STUArray s) Int16 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int32 Int32, Nullable Int32) =>
    UArrayMemo k Int32 (STUArray s) Int32 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Int64 Int64, Nullable Int64) =>
    UArrayMemo k Int64 (STUArray s) Int64 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word Word, Nullable Word) =>
    UArrayMemo k Word (STUArray s) Word (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word8 Word8, Nullable Word8) =>
    UArrayMemo k Word8 (STUArray s) Word8 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word16 Word16, Nullable Word16) =>
    UArrayMemo k Word16 (STUArray s) Word16 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word32 Word32, Nullable Word32) =>
    UArrayMemo k Word32 (STUArray s) Word32 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Word64 Word64, Nullable Word64) =>
    UArrayMemo k Word64 (STUArray s) Word64 (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Float Float, Nullable Float) =>
    UArrayMemo k Float (STUArray s) Float (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM

instance (Ix k, MaybeLike Double Double, Nullable Double) =>
    UArrayMemo k Double (STUArray s) Double (ST s) where
        {-# INLINE evalUArrayMemoM #-}
        evalUArrayMemoM = evalArrayMemoM
        {-# INLINE runUArrayMemoM #-}
        runUArrayMemoM = runArrayMemoM


instance (MaybeLike Char Char, Nullable Char) => STUArrayMemo Char where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Int Int, Nullable Int) => STUArrayMemo Int where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Int8 Int8, Nullable Int8) => STUArrayMemo Int8 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Int16 Int16, Nullable Int16) => STUArrayMemo Int16 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Int32 Int32, Nullable Int32) => STUArrayMemo Int32 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Int64 Int64, Nullable Int64) => STUArrayMemo Int64 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Word Word, Nullable Word) => STUArrayMemo Word where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Word8 Word8, Nullable Word8) => STUArrayMemo Word8 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Word16 Word16, Nullable Word16) => STUArrayMemo Word16 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Word32 Word32, Nullable Word32) => STUArrayMemo Word32 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Word64 Word64, Nullable Word64) => STUArrayMemo Word64 where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Float Float, Nullable Float) => STUArrayMemo Float where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu

instance (MaybeLike Double Double, Nullable Double) => STUArrayMemo Double where
    {-# INLINE evalSTUArrayMemo #-}
    evalSTUArrayMemo m lu = runST $ evalUArrayMemoM m lu
    {-# INLINE runSTUArrayMemo #-}
    runSTUArrayMemo m lu = runST $ runUArrayMemoM m lu