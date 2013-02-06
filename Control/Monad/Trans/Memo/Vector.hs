{- |
Module      :  Control.Monad.Trans.Memo.Vector
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

VectorCache - mutable-vector-based (`IO` and `ST` hosted) `MonadCache`

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, RankNTypes,
  UndecidableInstances, TypeFamilies, FunctionalDependencies #-}


module Control.Monad.Trans.Memo.Vector
 (

   -- * Generic VectorCache
   VectorCache(..),
   VectorMemo(..),

   -- * VectorCache for boxed types
   -- ** Using boxed ST Vector
   STVectorCache,
   STVector,
   runSTVectorMemoM,
   evalSTVectorMemoM,
   STVectorMemo(..),
   -- ** Using boxed IO Vector
   IOVectorCache,
   IOVector,
   runIOVectorMemoM,
   evalIOVectorMemoM,

   -- * VectorCache for unboxed types
   -- ** Using unboxed ST Vector
   STUVectorCache,
   STUVector,
   runSTUVectorMemoM,
   evalSTUVectorMemoM,
   STUVectorMemo(..),
   -- ** Using unboxed IO Vector
   IOUVectorCache,
   IOUVector,
   runIOUVectorMemoM,
   evalIOUVectorMemoM,

) where 


import Data.Int
import Data.Function
import Data.Maybe (Maybe(..))

import Data.Vector.Generic.Mutable (MVector, read, write)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Nullable
import Data.MaybeLike

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import System.IO

import Control.Monad.Memo.Class


newtype VectorCache vec m a = VectorCache { evalVectorCache :: vec -> m a }

instance (Functor m) => Functor (VectorCache vec m) where
    fmap f m = VectorCache $ \vec -> fmap f (evalVectorCache m vec)

instance (Applicative m) => Applicative (VectorCache vec m) where
    pure a   = VectorCache $ \_ -> pure a
    f <*> v = VectorCache $ \ vec -> evalVectorCache f vec <*> evalVectorCache v vec

instance (Monad m) => Monad (VectorCache vec m) where
    {-# INLINE return #-}
    return a = VectorCache $ \_ -> return a
    {-# INLINE (>>=) #-}
    m >>= k  = VectorCache $ \vec -> do
        a <- evalVectorCache m vec
        evalVectorCache (k a) vec
    {-# INLINE (>>) #-}
    m >> k   = m >>= \ _ -> k


instance (PrimMonad m, PrimState m ~ s, MVector vec e, MaybeLike e v) =>
    MonadCache Int v (VectorCache (vec s e) m) where
        {-# INLINE lookup #-}
        lookup k = VectorCache $ \vec -> do
                     mv <- read vec k
                     return $ if isNothing mv then Nothing else Just (fromJust mv)
        {-# INLINE add #-}
        add k v = VectorCache $ \vec -> write vec k (just v)

instance (PrimMonad m, PrimState m ~ s, MVector vec e, MaybeLike e v) =>
    MonadMemo Int v (VectorCache (vec s e) m) where
        {-# INLINE memo #-}
        memo f k = VectorCache $ \vec -> do
          e <- read vec k
          if isNothing e
             then do
               v <- evalVectorCache (f k) vec
               write vec k (just v)
               return v
             else return (fromJust e)


-- | Generic interface for running memo-function with `VectorCache` 
class (MonadMemo Int v (VectorCache (vec e) m)) =>
    VectorMemo v vec e m | m vec v -> e where
       evalVectorMemoM :: VectorCache (vec e) m a -> Int -> m a
       runVectorMemoM :: VectorCache (vec e) m a -> Int -> m (a, vec e)


-- VectorCache for boxed types
-- --------------------------

type STVectorCache s e = VectorCache (STVector s e) (ST s)

type STVector s = M.STVector s

evalSTVectorMemoM :: VectorMemo v (STVector s) e (ST s) =>
                     STVectorCache s e a -> Int -> ST s a
evalSTVectorMemoM = evalVectorMemoM

runSTVectorMemoM :: VectorMemo v (STVector s) e (ST s) =>
                    STVectorCache s e a -> Int -> ST s (a, STVector s e)
runSTVectorMemoM = runVectorMemoM

-- | Interface for evaluating memo-functions using boxed `STVector`
class STVectorMemo v e | v -> e, v -> v where
    evalSTVectorMemo :: (forall s. VectorMemo v (STVector s) e (ST s) =>
                        STVectorCache s e a) -> Int -> a
    runSTVectorMemo  :: (forall s. VectorMemo v (STVector s) e (ST s) =>
                        STVectorCache s e a) -> Int -> (a, V.Vector e)


type IOVectorCache e = VectorCache (IOVector e) IO

type IOVector = M.IOVector

evalIOVectorMemoM :: VectorMemo v IOVector e IO =>
                     IOVectorCache e a -> Int -> IO a
evalIOVectorMemoM = evalVectorMemoM

runIOVectorMemoM :: VectorMemo v IOVector e IO =>
                     IOVectorCache e a -> Int -> IO (a, IOVector e)
runIOVectorMemoM = runVectorMemoM


-- VectorCache for unboxed types
-- ----------------------------

type STUVectorCache s e = VectorCache (STUVector s e) (ST s)

type STUVector s = UM.STVector s

-- | Computes result using unboxed `UMV.MVector` within `ST` monad
evalSTUVectorMemoM :: VectorMemo v (STUVector s) e (ST s) =>
                     STUVectorCache s e a -> Int -> ST s a
evalSTUVectorMemoM = evalVectorMemoM

-- | Computes result and the final cache using unboxed `UMV.MVector` vector within `ST` monad
runSTUVectorMemoM :: VectorMemo v (STUVector s) e (ST s) =>
                    STUVectorCache s e a -> Int -> ST s (a, STUVector s e)
runSTUVectorMemoM = runVectorMemoM


type IOUVectorCache e = VectorCache (IOUVector e) IO

type IOUVector = UM.IOVector

-- | Computes result using unboxed `UMV.MVector` within `IO` monad
evalIOUVectorMemoM :: VectorMemo v IOUVector e IO =>
                      IOUVectorCache e a -> Int -> IO a
evalIOUVectorMemoM = evalVectorMemoM

-- | Computes result and the final cache using unboxed `UMV.MVector` within `IO` monad
runIOUVectorMemoM :: VectorMemo v IOUVector e IO =>
                     IOUVectorCache e a -> Int -> IO (a, IOUVector e)
runIOUVectorMemoM = runVectorMemoM


-- | Interface for evaluating memo-functions using unboxed `UMV.MVector`
class STUVectorMemo v e | v -> e, e -> v where
    evalSTUVectorMemo :: (forall s. VectorMemo v (STUVector s) e (ST s) =>
                         STUVectorCache s e a) -> Int -> a
    runSTUVectorMemo  :: (forall s. VectorMemo v (STUVector s) e (ST s) =>
                         STUVectorCache s e a) -> Int -> (a, UV.Vector e)
