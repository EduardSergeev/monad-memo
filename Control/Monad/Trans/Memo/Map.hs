{- |
Module      :  Control.Monad.Trans.Memo.Map
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Specialization of `MemoStateT` with `Data.Map` as a container

-}

{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Trans.Memo.Map
(

   -- * MemoT monad transformer
   MemoT,
   runMemoT,
   evalMemoT,
   startRunMemoT,
   startEvalMemoT,
   -- * Memo monad
   Memo,
   runMemo,
   evalMemo,
   startRunMemo,
   startEvalMemo,

) where

import Control.Monad.Identity
import Control.Monad.Trans.Memo.State

import Data.MapLike.Instances()
import qualified Data.Map as M


-- | Memoization monad transformer which uses `Data.Map` as a cache container
type MemoT k v = MemoStateT (M.Map k v) k v

-- | Given an initial cache, compute the result of a memoized computation
-- along with the final state of the cache
runMemoT :: Monad m => MemoT k v m a -> M.Map k v -> m (a, M.Map k v)
runMemoT = runMemoStateT

-- | Given an initial state, compute the result of a memoized computation
-- discarding the final state of the cache
evalMemoT :: Monad m => MemoT k v m a -> M.Map k v -> m a
evalMemoT = evalMemoStateT

-- | Compute the result of memoized computation along with the final state of the cache.
-- This function uses empty `M.Map` as an initial state
startRunMemoT :: Monad m => MemoT k v m a -> m (a, M.Map k v)
startRunMemoT = (`runMemoT` M.empty)

-- | Compute the result of a memoized computation discarding the final state of the cache.
-- This function uses empty `M.Map` as an initial state
startEvalMemoT :: Monad m => MemoT k v m a -> m a
startEvalMemoT = (`evalMemoT` M.empty)


-- | Memoization monad which uses `Data.Map` as a cache container
type Memo k v = MemoT k v Identity

-- | Given an initial cache, compute the result of a memoized computation
-- along with the final state of the cache
runMemo :: Memo k v a -> M.Map k v -> (a, M.Map k v)
runMemo = runMemoState

-- | Given an initial state, compute the result of a memoized computation
-- discarding the final state of the cache
evalMemo :: Memo k v a -> M.Map k v -> a
evalMemo = evalMemoState

-- | Compute the result of memoized computation along with the final state of the cache.
-- This function uses empty `M.Map` as an initial state
startRunMemo :: Memo k v a -> (a, M.Map k v)
startRunMemo = (`runMemo` M.empty)

-- | Compute the result of a memoized computation discarding the final state of the cache.
-- This function uses empty `M.Map` as an initial state
startEvalMemo :: Memo k v a -> a
startEvalMemo = (`evalMemo` M.empty)
