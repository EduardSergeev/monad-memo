{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts,
  UndecidableInstances, TypeFamilies #-}

module Control.Monad.Memo.Vector.Expandable
 (

   -- * VectorCache for boxed types
   Vector,
   VectorCache,
   VectorMemo,
   startEvalVectorMemo,
   startRunVectorMemo,
   -- * UVectorCache for unboxed types
   UVector,
   UVectorCache,
   UVectorMemo,
   startEvalUVectorMemo,
   startRunUVectorMemo

) where 

import Data.Int
import Data.Eq
import Data.Ord
import Data.Function
import Prelude (Num(..))
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
import Control.Monad.Trans.Memo.StateCache


newtype Cache c s e m a = Cache { toStateCache :: StateCache (c s e) m a }

instance Functor m => Functor (Cache c s e m) where
    {-# INLINE fmap #-}
    fmap f m = Cache $ fmap f (toStateCache m)

instance (Functor m, Monad m) => Applicative (Cache c s e m) where
    {-# INLINE pure #-}
    pure = Cache . pure
    {-# INLINE (<*>) #-}
    a <*> b = Cache (toStateCache a <*> toStateCache b)

instance Monad m => Monad (Cache c s e m) where
    {-# INLINE return #-}
    return = Cache . return
    {-# INLINE (>>=) #-}
    m >>= f = Cache ((toStateCache m) >>= toStateCache . f)

instance MonadTrans (Cache c s e) where
    {-# INLINE lift #-}
    lift = Cache . lift

instance MonadFix m => MonadFix (Cache c s e m) where
    {-# INLINE mfix #-}
    mfix f = Cache $ mfix $ \a -> toStateCache (f a)


instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadCache Int v (Cache c s e m) where
        {-# INLINE lookup #-}
        lookup k = do
          c <- Cache container
          e <- lift $ cacheRead c k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- Cache container
          c' <- lift $ cacheWrite c k (just v)
          Cache $ setContainer c'

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadMemo Int v (Cache c s e m) where
        {-# INLINE memo #-}
        memo f k = do
          c <- Cache container
          let l = length c
              d = k + 1 - l
          if d > 0
            then do
              c' <- lift $ expand c l d
              Cache $ setContainer c'
              v <- f k
              c'' <- Cache container
              lift $ unsafeWrite c'' k (just v)
              return v
            else do
              e <- lift $ cacheRead c k
              if isNothing e
                 then do
                   v <- f k
                   c' <- Cache container
                   lift $ unsafeWrite c' k (just v)
                   return v
                 else return (fromJust e)


-- VectorCache for boxed types
-- --------------------------
type Vector = M.MVector

type VectorCache = Cache Vector

class MaybeLike e v => VectorMemo v e | v -> e

startEvalVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                       VectorCache (PrimState m) e m a -> m a
{-# INLINE startEvalVectorMemo #-}
startEvalVectorMemo = genericStartEvalVectorMemo

startRunVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                      VectorCache (PrimState m) e m a -> m (a, Vector (PrimState m) e)
{-# INLINE startRunVectorMemo #-}
startRunVectorMemo = genericStartRunUVectorMemo


-- VectorCache for unboxed types
-- ----------------------------
type UVector = UM.MVector

type UVectorCache = Cache UVector

class MaybeLike e v => UVectorMemo v e | v -> e

startEvalUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                        UVectorCache (PrimState m) e m a -> m a
{-# INLINE startEvalUVectorMemo #-}
startEvalUVectorMemo = genericStartEvalVectorMemo

startRunUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                       UVectorCache (PrimState m) e m a -> m (a, UVector (PrimState m) e)
{-# INLINE startRunUVectorMemo #-}
startRunUVectorMemo = genericStartRunUVectorMemo


genericStartEvalVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                              Cache c (PrimState m) e m a -> m a
{-# INLINE genericStartEvalVectorMemo #-}
genericStartEvalVectorMemo m = do
  (a,_) <- genericStartRunUVectorMemo m
  return a

genericStartRunUVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                              Cache c (PrimState m) e m a -> m (a, c (PrimState m) e)
{-# INLINE genericStartRunUVectorMemo #-}
genericStartRunUVectorMemo m = do
  c <- replicate 0 nothing
  runStateCache (toStateCache m) c


{-# INLINE cacheRead #-}
cacheRead c k =
    if k >= length c
        then return nothing
        else unsafeRead c k

{-# INLINE cacheWrite #-}
cacheWrite c k e = do
  c' <- if d > 0
           then expand c l d
           else return c
  unsafeWrite c' k e
  return c'
    where
      l = length c
      d = k + 1 - l

{-# INLINE expand #-}
expand c l d = do
  uc <- unsafeGrow c toGrow
  unsafeWrite uc l nothing
  initialise uc 1
      where
        toGrow = d `max` (l * 2)
        {-# INLINE initialise #-}
        initialise c i | i == toGrow = return c
        initialise c i = do
                 let n = i `min` (toGrow-i)
                     t = unsafeSlice (l+i) n c
                     s = unsafeSlice l n c
                 unsafeCopy t s
                 initialise c (i+n)
