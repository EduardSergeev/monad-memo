{- |
Module      :  Control.Monad.Trans.Memo.Vector.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Default instances for `VectorMemo`, `STVectorMemo` and `STUVectorMemo`

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, UndecidableInstances,
  FlexibleContexts, FlexibleInstances #-}


module Control.Monad.Trans.Memo.Vector.Instances
(
  
  module Control.Monad.Trans.Memo.Vector,
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

import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Nullable
import Data.MaybeLike

import System.IO
import Control.Monad
import Control.Monad.ST

import Control.Monad.Trans.Memo.Vector



instance (MaybeLike (Maybe v) v, Nullable (Maybe v)) =>
    VectorMemo v IOVector (Maybe v) IO where
        {-# INLINE evalVectorMemoM #-}
        evalVectorMemoM m n = do
          vec <- M.replicate n null
          evalVectorCache m vec
        {-# INLINE runVectorMemoM #-}
        runVectorMemoM m n = do
          vec <- M.replicate n null
          a <- evalVectorCache m vec
          return (a, vec)

instance (MaybeLike (Maybe v) v, Nullable (Maybe v)) =>
    VectorMemo v (STVector s) (Maybe v) (ST s) where
        {-# INLINE evalVectorMemoM #-}
        evalVectorMemoM m n = do
          vec <- M.replicate n null
          evalVectorCache m vec
        {-# INLINE runVectorMemoM #-}
        runVectorMemoM m n = do
          vec <- M.replicate n null
          a <- evalVectorCache m vec
          return (a, vec)

instance (MaybeLike v v, Nullable v, UV.Unbox v, MVector UM.MVector v) =>
    VectorMemo v IOUVector v IO where
        {-# INLINE evalVectorMemoM #-}
        evalVectorMemoM m n = do
          vec <- UM.replicate n null
          evalVectorCache m vec
        {-# INLINE runVectorMemoM #-}
        runVectorMemoM m n = do
          vec <- UM.replicate n null
          a <- evalVectorCache m vec
          return (a, vec)

instance (MaybeLike v v, Nullable v, UV.Unbox v, MVector UM.MVector v) =>
    VectorMemo v (STUVector s) v (ST s) where
        {-# INLINE evalVectorMemoM #-}
        evalVectorMemoM m n = do
          vec <- UM.replicate n null
          evalVectorCache m vec
        {-# INLINE runVectorMemoM #-}
        runVectorMemoM m n = do
          vec <- UM.replicate n null
          a <- evalVectorCache m vec
          return (a, vec)


instance STVectorMemo v (Maybe v) where
    {-# INLINE evalSTVectorMemo #-}
    evalSTVectorMemo m n = runST $ evalVectorMemoM m n
    {-# INLINE runSTVectorMemo #-}
    runSTVectorMemo m n = runST $  do
      (a, vec) <- runVectorMemoM m n
      ivec <- V.freeze vec
      return (a, ivec)


instance (MaybeLike v v, Nullable v, UM.Unbox v) => STUVectorMemo v v where
    {-# INLINE evalSTUVectorMemo #-}
    evalSTUVectorMemo m n = runST $ evalVectorMemoM m n
    {-# INLINE runSTUVectorMemo #-}
    runSTUVectorMemo m n = runST $ do
      (a, vec) <- runVectorMemoM m n
      ivec <- UV.freeze vec
      return (a, ivec)