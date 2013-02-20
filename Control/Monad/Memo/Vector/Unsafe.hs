{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

VectorCache - mutable-vector-based `MonadCache` with unsafe operations.

This is a version of "Control.Monad.Memo.Mutable.Vector" but implemented using /unsafe*/ vector operations.
Faster than default implementation but you must be sure that your code doesn't try to read/write outside vector
-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts, TypeFamilies,
  UndecidableInstances #-}

module Control.Monad.Memo.Vector.Unsafe
 (

   -- * VectorCache for boxed types
   Vector,
   VectorCache,
   VectorMemo,
   unsafeEvalVectorMemo,
   unsafeRunVectorMemo,
   -- * UVectorCache for unboxed types
   UVector,
   UVectorCache,
   UVectorMemo,
   unsafeEvalUVectorMemo,
   unsafeRunUVectorMemo

) where 

import Data.Function
import Data.Int
import Data.Maybe (Maybe(..))
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Primitive

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.ReaderCache

newtype Cache c s e m a = Cache { toReaderCache :: ReaderCache (c s e) m a }

instance Functor m => Functor (Cache c s e m) where
    {-# INLINE fmap #-}
    fmap f m = Cache $ fmap f (toReaderCache m)

instance (Functor m, Applicative m) => Applicative (Cache c s e m) where
    {-# INLINE pure #-}
    pure = Cache . pure
    {-# INLINE (<*>) #-}
    a <*> b = Cache (toReaderCache a <*> toReaderCache b)

instance Monad m => Monad (Cache c s e m) where
    {-# INLINE return #-}
    return = Cache . return
    {-# INLINE (>>=) #-}
    m >>= f = Cache $ toReaderCache m >>= toReaderCache . f

instance MonadTrans (Cache c s e) where
    {-# INLINE lift #-}
    lift = Cache . lift

instance MonadFix m => MonadFix (Cache c s e m) where
    {-# INLINE mfix #-}
    mfix f = Cache $ mfix $ \a -> toReaderCache (f a)


instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadCache Int v (Cache c s e m) where
        {-# INLINE lookup #-}
        lookup k = do
          c <- Cache container
          e <- lift $ unsafeRead c k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- Cache container
          lift $ unsafeWrite c k (just v)

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadMemo Int v (Cache c s e m) where
        {-# INLINE memo #-}
        memo f k = do
          c <- Cache container
          e <- lift $ unsafeRead c k
          if isNothing e
             then do
               v <- f k
               lift $ unsafeWrite c k (just v)
               return v
             else return (fromJust e)


-- VectorCache for boxed types
-- --------------------------
type Vector = M.MVector

type VectorCache = Cache Vector

class MaybeLike e v => VectorMemo v e | v -> e

unsafeEvalVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                        VectorCache (PrimState m) e m a -> Int -> m a
{-# INLINE unsafeEvalVectorMemo #-}
unsafeEvalVectorMemo = genericUnsafeEvalVectorMemo

unsafeRunVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                       VectorCache (PrimState m) e m a -> Int -> m (a, Vector (PrimState m) e)
{-# INLINE unsafeRunVectorMemo #-}
unsafeRunVectorMemo = genericUnsafeRunVectorMemo


-- VectorCache for unboxed types
-- ----------------------------
type UVector = UM.MVector

type UVectorCache = Cache UVector

class MaybeLike e v => UVectorMemo v e | v -> e

unsafeEvalUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                         UVectorCache (PrimState m) e m a -> Int -> m a
{-# INLINE unsafeEvalUVectorMemo #-}
unsafeEvalUVectorMemo = genericUnsafeEvalVectorMemo

unsafeRunUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                        UVectorCache (PrimState m) e m a -> Int -> m (a, UVector (PrimState m) e)
{-# INLINE unsafeRunUVectorMemo #-}
unsafeRunUVectorMemo = genericUnsafeRunVectorMemo


genericUnsafeEvalVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                               Cache c (PrimState m) e m a -> Int -> m a
{-# INLINE genericUnsafeEvalVectorMemo #-}
genericUnsafeEvalVectorMemo m n = do
  c <- replicate n nothing
  evalReaderCache (toReaderCache m) c

genericUnsafeRunVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                              Cache c (PrimState m) e m a -> Int -> m (a, c (PrimState m) e)
{-# INLINE genericUnsafeRunVectorMemo #-}
genericUnsafeRunVectorMemo m n = do
  c <- replicate n nothing
  a <- evalReaderCache (toReaderCache m) c
  return (a, c)
