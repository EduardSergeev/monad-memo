{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

VectorCache - mutable-vector-based `MonadCache` with unsafe operations.

This is a version of "Control.Monad.Memo.Mutable.Vector" but implemented using /unsafe*/ vector operations.
Faster than default implementation but you must be sure that your code doesn't try to read/write outside vector boundaries.

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts, TypeFamilies,
  UndecidableInstances, TypeSynonymInstances #-}

module Control.Monad.Memo.Vector.Unsafe
 (

   -- * VectorCache for boxed types
   VectorCache,
   VectorMemo,
   unsafeEvalVectorMemo,
   unsafeRunVectorMemo,
   -- * UVectorCache for unboxed types
   UVectorCache,
   UVectorMemo,
   unsafeEvalUVectorMemo,
   unsafeRunUVectorMemo,
   -- * Generic functions for VectorCache
   Container(..),
   Cache,
   genericUnsafeEvalVectorMemo,
   genericUnsafeRunVectorMemo


) where 

import Data.Function
import Data.Int
import Data.Maybe (Maybe(..))
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Primitive

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.ReaderCache


newtype Container vec = Container { toVector :: vec }

-- | Generic Vector-based memo cache
type Cache vec k e = ReaderCache (Container (vec k e))

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadCache Int v (Cache c s e m) where
        {-# INLINE lookup #-}
        lookup k = do
          c <- container
          e <- lift $ unsafeRead (toVector c) k
          return (if isNothing e then Nothing else Just (fromJust e))
        {-# INLINE add #-}
        add k v = do 
          c <- container
          lift $ unsafeWrite (toVector c) k (just v)

instance (PrimMonad m, PrimState m ~ s, MaybeLike e v, MVector c e) =>
    MonadMemo Int v (Cache c s e m) where
        {-# INLINE memo #-}
        memo f k = do
          c <- container
          e <- lift $ unsafeRead (toVector c) k
          if isNothing e
             then do
               v <- f k
               lift $ unsafeWrite (toVector c) k (just v)
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

-- | Evaluate computation using mutable boxed vector and unsafe operations
--
-- Vector length must covers all possible keys used in computation
-- otherwise the behaviour is undefined (i.e. segfault)
unsafeEvalVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                        VectorCache (PrimState m) e m a -- ^memoized computation
                     -> Int                             -- ^vector length 
                     -> m a                             -- ^result
{-# INLINE unsafeEvalVectorMemo #-}
unsafeEvalVectorMemo = genericUnsafeEvalVectorMemo

-- | Evaluate computation using mutable boxed vector and unsafe operations.
-- It also returns the final content of the vector cache
--
-- Vector length must covers all possible keys used in computation
-- otherwise the behaviour is undefined (i.e. segfault)
unsafeRunVectorMemo :: (PrimMonad m, VectorMemo v e) =>
                       VectorCache (PrimState m) e m a -- ^memoized computation
                    -> Int                             -- ^vector length
                    -> m (a, Vector (PrimState m) e)   -- ^result and final vector cache
{-# INLINE unsafeRunVectorMemo #-}
unsafeRunVectorMemo = genericUnsafeRunVectorMemo


-- VectorCache for unboxed types
-- ----------------------------

-- | Unboxed vector
type UVector = UM.MVector

-- | `MonadCache` based on unboxed vector
type UVectorCache s e = Cache UVector s e

-- | This is just to be able to infer the type of the `UVectorCache` element
class MaybeLike e v => UVectorMemo v e | v -> e

-- | Evaluate computation using mutable unboxed vector and unsafe operations
--
-- Vector length must covers all possible keys used in computation
-- otherwise the behaviour is undefined (i.e. segfault)
unsafeEvalUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                         UVectorCache (PrimState m) e m a -- ^memoized computation
                      -> Int                              -- ^vector length
                      -> m a                              -- ^result
{-# INLINE unsafeEvalUVectorMemo #-}
unsafeEvalUVectorMemo = genericUnsafeEvalVectorMemo

-- | Evaluate computation using mutable boxed vector and unsafe operations.
-- It also returns the final content of the vector cache
--
-- Vector length must covers all possible keys used in computation
-- otherwise the behaviour is undefined (i.e. segfault)
unsafeRunUVectorMemo :: (PrimMonad m, UVectorMemo v e, MVector UVector e) =>
                        UVectorCache (PrimState m) e m a -- ^memoized computation
                     -> Int                              -- ^vector length
                     -> m (a, UVector (PrimState m) e)   -- ^result and final vector cache
{-# INLINE unsafeRunUVectorMemo #-}
unsafeRunUVectorMemo = genericUnsafeRunVectorMemo


genericUnsafeEvalVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                               Cache c (PrimState m) e m a -> Int -> m a
{-# INLINE genericUnsafeEvalVectorMemo #-}
genericUnsafeEvalVectorMemo m n = do
  vec <- replicate n nothing
  evalReaderCache m (Container vec)

genericUnsafeRunVectorMemo :: (MaybeLike e v, PrimMonad m, MVector c e) =>
                              Cache c (PrimState m) e m a -> Int -> m (a, c (PrimState m) e)
{-# INLINE genericUnsafeRunVectorMemo #-}
genericUnsafeRunVectorMemo m n = do
  vec <- replicate n nothing
  a <- evalReaderCache m (Container vec)
  return (a, vec)
