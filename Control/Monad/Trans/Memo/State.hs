{- |
Module      :  Control.Monad.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Defines "MemoStateT" - generalized (to any "Data.MapLike" content) memoization monad transformer

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances,
  GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Memo.State
(
 
    MemoStateT(..),
    runMemoStateT,
    evalMemoStateT,

    MemoState,
    runMemoState,
    evalMemoState,

) where


import Data.Tuple
import Data.Ord
import Data.Function
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Identity

import qualified Data.MapLike as M
import Control.Monad.Memo.Class


-- MonadMemo and MonadCache implementation using std. 'Control.Monad.State' transformer
-- with generic 'Data.MapLike' container for a cache
newtype MemoStateT c k v m a = MemoStateT { toStateT :: StateT c m a }
    deriving (Functor, Applicative, Alternative,
                     Monad, MonadPlus, MonadFix)

runMemoStateT :: MemoStateT c k v m a -> c -> m (a, c)
runMemoStateT = runStateT . toStateT

evalMemoStateT :: (Monad m) => MemoStateT c k v m a -> c -> m a
evalMemoStateT m s = runMemoStateT m s >>= return . fst


type MemoState c k v = MemoStateT c k v Identity

runMemoState :: MemoState c k v a -> c -> (a, c)
runMemoState m = runIdentity . runMemoStateT m

evalMemoState :: MemoState c k v a -> c -> a
evalMemoState m = runIdentity . evalMemoStateT m


instance (Monad m, M.MapLike c k v) => MonadCache k v (MemoStateT c k v m) where
    lookup k = MemoStateT $ get >>= return . M.lookup k
    add k v  = MemoStateT $ modify $ \m -> M.add k v m

instance (Monad m, M.MapLike c k v) => MonadMemo k v (MemoStateT c k v m) where
    memo = memol0


instance (MonadIO m) => MonadIO (MemoStateT l k v m) where
    liftIO = lift . liftIO

instance MonadTrans (MemoStateT l k v) where
    lift = MemoStateT . lift