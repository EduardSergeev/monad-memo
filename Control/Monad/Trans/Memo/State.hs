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
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

import qualified Data.MapLike as M
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.StateCache


-- MonadMemo and MonadCache implementation using std. 'Control.Monad.State' transformer
-- with generic 'Data.MapLike' container for a cache
newtype MemoStateT c k v m a = MemoStateT { toStateT :: StateCache c m a }
    deriving (Functor, Applicative, Alternative,
              Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

runMemoStateT :: MemoStateT c k v m a -> c -> m (a, c)
runMemoStateT = runStateCache . toStateT

evalMemoStateT :: (Monad m) => MemoStateT c k v m a -> c -> m a
evalMemoStateT m s = runMemoStateT m s >>= return . fst


type MemoState c k v = MemoStateT c k v Identity

runMemoState :: MemoState c k v a -> c -> (a, c)
runMemoState m = runIdentity . runMemoStateT m

evalMemoState :: MemoState c k v a -> c -> a
evalMemoState m = runIdentity . evalMemoStateT m


instance (Monad m, M.MapLike c k v) => MonadCache k v (MemoStateT c k v m) where
    {-# INLINE lookup #-}
    lookup k = MemoStateT $ container >>= return . M.lookup k
    {-# INLINE add #-}
    add k v  = MemoStateT $ do
                 m <- container
                 setContainer $ M.add k v m

instance (Monad m, M.MapLike c k v) => MonadMemo k v (MemoStateT c k v m) where
    {-# INLINE memo #-}
    memo = memol0