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
  FlexibleInstances, FlexibleContexts, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Control.Monad.Memo.Vector
 (

   -- * VectorCache for boxed types
   Vector,
   VectorCache,
   VectorMemo,
   evalVectorMemo,
   runVectorMemo,
   -- * UVectorCache for unboxed types
   UVector,
   UVectorCache,
   UVectorMemo,
   evalUVectorMemo,
   runUVectorMemo

) where 

import Data.Int
import Data.Function
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
    a <*> b = Cache $ toReaderCache a <*> toReaderCache b

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
          e <- lift $ read c k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- Cache container
          lift $ write c k (just v)

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadMemo Int v (Cache c s e m) where
        {-# INLINE memo #-}
        memo f k = do
          c <- Cache container
          e <- lift $ read c k
          if isNothing e
            then do
              v <- f k
              lift $ write c k (just v)
              return v
            else return (fromJust e) 

-- VectorCache for boxed types
-- --------------------------
type Vector = M.MVector

type VectorCache = Cache Vector

class MaybeLike e v => VectorMemo v e | v -> e

evalVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                  VectorCache (PrimState m) e m a -> Int -> m a
{-# INLINE evalVectorMemo #-}
evalVectorMemo = genericEvalVectorMemo

runVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                 VectorCache (PrimState m) e m a -> Int -> m (a, Vector (PrimState m) e)
{-# INLINE runVectorMemo #-}
runVectorMemo = genericRunVectorMemo

-- VectorCache for unboxed types
-- ----------------------------
type UVector = UM.MVector

type UVectorCache = Cache UVector

class MaybeLike e v => UVectorMemo v e | v -> e

evalUVectorMemo :: (PrimMonad m, MVector UVector e, UVectorMemo v e) =>
                   UVectorCache (PrimState m) e m a -> Int -> m a
{-# INLINE evalUVectorMemo #-}
evalUVectorMemo = genericEvalVectorMemo

runUVectorMemo :: (PrimMonad m, MVector UVector e, UVectorMemo v e) =>
                  UVectorCache (PrimState m) e m a -> Int -> m (a, UVector (PrimState m) e)
{-# INLINE runUVectorMemo #-}
runUVectorMemo = genericRunVectorMemo


genericEvalVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                         Cache c (PrimState m) e m a -> Int -> m a
{-# INLINE genericEvalVectorMemo #-}
genericEvalVectorMemo m n = do
  c <- replicate n nothing
  evalReaderCache (toReaderCache m) c

genericRunVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                        Cache c (PrimState m) e m a -> Int -> m (a, c (PrimState m) e)
{-# INLINE genericRunVectorMemo #-}
genericRunVectorMemo m n = do
  c <- replicate n nothing
  a <- evalReaderCache (toReaderCache m) c
  return (a, c)