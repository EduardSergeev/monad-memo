{- |
Module      :  Control.Monad.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Defines "MemoStateT" - generalized (to any "Data.MapLike" content) memoization monad transformer

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  FlexibleInstances, TypeSynonymInstances #-}

module Control.Monad.Trans.Memo.State
(
 
    -- * MemoStateT monad transformer
    MemoStateT(..),
    runMemoStateT,
    evalMemoStateT,
    -- * MemoState monad
    MemoState,
    runMemoState,
    evalMemoState,
    -- * Internal
    Container(..)

) where


import Data.Tuple
import Data.Function
import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.MapLike as M
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.StateCache


newtype Container s = Container { toState :: s }

-- | Memoization monad transformer based on `StateCache`
-- to be used with pure cache containers which support `M.MapLike` interface
type MemoStateT s k v = StateCache (Container s)


-- | Returns the pair of the result of `MonadMemo` computation
-- along with the final state of the internal pure container wrapped in monad
runMemoStateT :: Monad m => MemoStateT s k v m a -> s -> m (a, s)
runMemoStateT m s = do
  (a, c) <- runStateCache m (Container s)
  return (a, toState c)

-- | Returns the result of `MonadMemo` computation wrapped in monad.
-- This function discards the cache
evalMemoStateT :: Monad m => MemoStateT c k v m a -> c -> m a
evalMemoStateT m s = runMemoStateT m s >>= return . fst


-- | Memoization monad based on `StateCache`
-- to be used with pure cache containers which support `M.MapLike` interface
type MemoState c k v = MemoStateT c k v Identity

-- | Returns the pair of the result of `MonadMemo` computation
-- along with the final state of the internal pure container
runMemoState :: MemoState c k v a -> c -> (a, c)
runMemoState m = runIdentity . runMemoStateT m

-- | Returns the result of `MonadMemo` computation discarding the cache
evalMemoState :: MemoState c k v a -> c -> a
evalMemoState m = runIdentity . evalMemoStateT m


instance (Monad m, M.MapLike c k v) => MonadCache k v (MemoStateT c k v m) where
    {-# INLINE lookup #-}
    lookup k = container >>= return . M.lookup k . toState
    {-# INLINE add #-}
    add k v  = container >>= setContainer . Container . M.add k v . toState

instance (Monad m, M.MapLike c k v) => MonadMemo k v (MemoStateT c k v m) where
    {-# INLINE memo #-}
    memo = memol0