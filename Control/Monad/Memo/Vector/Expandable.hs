{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Vector-based `MonadCache` implementation which dynamically expands the vector during the computation to accomodate all requested keys.
This implementation does not require to specify the length of the vector up front, but may be slower than "Control.Monad.Memo.Vector".

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
  UndecidableInstances, TypeFamilies #-}

module Control.Monad.Memo.Vector.Expandable
 (

   -- * VectorCache for boxed types
   VectorCache,
   VectorMemo,
   startEvalVectorMemo,
   startRunVectorMemo,
   -- * UVectorCache for unboxed types
   UVectorCache,
   UVectorMemo,
   startEvalUVectorMemo,
   startRunUVectorMemo,
   -- * Generic functions for VectorCache
   Container(..),
   Cache,
   genericStartEvalVectorMemo,
   genericStartRunVectorMemo

) where 

import Data.Int
import Data.Eq
import Data.Ord
import Data.Function
import Prelude (Num(..))
import Data.Maybe (Maybe(..))
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Primitive

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.StateCache


newtype Container vec = Container { toVector :: vec }

-- | Generic Vector-based memo cache
type Cache vec k e = StateCache (Container (vec k e))

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadCache Int v (Cache c s e m) where
        {-# INLINE lookup #-}
        lookup k = do
          c <- container
          e <- lift $ cacheRead (toVector c) k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- container
          v' <- lift $ cacheWrite (toVector c) k (just v)
          setContainer (Container v')

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadMemo Int v (Cache c s e m) where
        {-# INLINE memo #-}
        memo f k = do
          Container vec <- container
          let l = length vec
              d = k + 1 - l
          if d > 0
            then do
              vec' <- lift $ expand vec l d
              setContainer (Container vec')
              v <- f k
              Container vec'' <- container
              lift $ unsafeWrite vec'' k (just v)
              return v
            else do
              e <- lift $ cacheRead vec k
              if isNothing e
                 then do
                   v <- f k
                   Container vec' <- container
                   lift $ unsafeWrite vec' k (just v)
                   return v
                 else return (fromJust e)


-- VectorCache for boxed types
-- --------------------------

-- | Boxed vector
type Vector = M.MVector

-- | `MonadCache` based on boxed vector
type VectorCache s e = Cache Vector s e

-- | This is just to be able to infer the type of the `VectorCache` element.
class MaybeLike e v => VectorMemo v e | v -> e

-- | Evaluate computation using mutable boxed vector which dynamically grows to accomodate all requested keys 
startEvalVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                       VectorCache (PrimState m) e m a -> m a
{-# INLINE startEvalVectorMemo #-}
startEvalVectorMemo = genericStartEvalVectorMemo

-- | Evaluate computation using mutable boxed vector
-- which dynamically grows to accomodate all requested keys. 
-- This function also returns the final content of the vector cache
startRunVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                      VectorCache (PrimState m) e m a -> m (a, Vector (PrimState m) e)
{-# INLINE startRunVectorMemo #-}
startRunVectorMemo = genericStartRunVectorMemo


-- VectorCache for unboxed types
-- ----------------------------

-- | Unboxed vector
type UVector = UM.MVector

-- | `MonadCache` based on unboxed vector
type UVectorCache s e = Cache UVector s e

-- | This is just to be able to infer the type of the `UVectorCache` element.
class MaybeLike e v => UVectorMemo v e | v -> e

-- | Evaluate computation using mutable unboxed vector
-- which dynamically grows to accomodate all requested keys 
startEvalUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                        UVectorCache (PrimState m) e m a -> m a
{-# INLINE startEvalUVectorMemo #-}
startEvalUVectorMemo = genericStartEvalVectorMemo

-- | Evaluate computation using mutable unboxed vector
-- which dynamically grows to accomodate all requested keys.
-- This function also returns the final content of the vector cache
startRunUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                       UVectorCache (PrimState m) e m a -> m (a, UVector (PrimState m) e)
{-# INLINE startRunUVectorMemo #-}
startRunUVectorMemo = genericStartRunVectorMemo


genericStartEvalVectorMemo :: (MaybeLike e v, PrimMonad m, MVector vec e) =>
                              Cache vec (PrimState m) e m a -> m a
{-# INLINE genericStartEvalVectorMemo #-}
genericStartEvalVectorMemo m = do
  (a,_) <- genericStartRunVectorMemo m
  return a

genericStartRunVectorMemo :: (MaybeLike e v, PrimMonad m, MVector vec e) =>
                             Cache vec (PrimState m) e m a -> m (a, vec (PrimState m) e)
{-# INLINE genericStartRunVectorMemo #-}
genericStartRunVectorMemo m = do
  vec <- replicate 0 nothing
  (a, c) <- runStateCache m (Container vec)
  return (a, toVector c)

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
