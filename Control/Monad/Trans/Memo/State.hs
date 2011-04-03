{- |
Module      :  Control.Monad.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Defines "MemoStateT" - generalized (to any "Data.MapLike" content) memoization monad transformer
-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}

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



newtype MemoStateT c k v m a = MemoStateT { toStateT :: StateT (c k v) m a }

runMemoStateT :: MemoStateT c k v m a -> c k v -> m (a, c k v)
runMemoStateT = runStateT . toStateT

evalMemoStateT :: (Monad m) => MemoStateT c k v m a -> c k v -> m a
evalMemoStateT m s = runMemoStateT m s >>= return . fst


type MemoState c k v = MemoStateT c k v Identity

runMemoState :: MemoState c k v a -> c k v -> (a, c k v)
runMemoState m = runIdentity . runMemoStateT m

evalMemoState :: MemoState c k v a -> c k v -> a
evalMemoState m = runIdentity . evalMemoStateT m



instance (Functor m) => Functor (MemoStateT c k v m) where
    fmap f m = MemoStateT $ fmap f (toStateT m)

instance (Functor m, Monad m) => Applicative (MemoStateT c k v m) where
    pure  = return 
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (MemoStateT l k v m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (MemoStateT l k v m) where
    return = MemoStateT . return
    m >>= k = MemoStateT $ (toStateT m) >>= (toStateT . k) 
    m >> n = MemoStateT $ (toStateT m) >> (toStateT n) 

instance (MonadPlus m) => MonadPlus (MemoStateT l k v m) where
    mzero       = MemoStateT mzero
    m `mplus` n = MemoStateT $ toStateT m `mplus` toStateT n

instance (MonadFix m) => MonadFix (MemoStateT l k v m) where
    mfix f = MemoStateT $ mfix (toStateT . f)


instance (Monad m, M.MapLike c k v) => MonadCache k v (MemoStateT c k v m) where
    lookup k = MemoStateT $ get >>= return . M.lookup k
    add k v  = MemoStateT $ modify $ \m -> M.add k v m

instance (Monad m, M.MapLike c k v) => MonadMemo k v (MemoStateT c k v m) where
    memo = memol0


instance (MonadIO m) => MonadIO (MemoStateT l k v m) where
    liftIO = lift . liftIO

instance MonadTrans (MemoStateT l k v) where
    lift = MemoStateT . lift