{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction,
MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Trans.Memo.Strict
(
 
MemoT(..),
runMemoT,
startRunMemoT,
evalMemoT,
startEvalMemoT,

Memo(..),
runMemo,
startRunMemo,
evalMemo,
startEvalMemo,

) where


import Data.Tuple
import Data.Ord
import Data.Maybe
import Data.Function
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.Map as M 

import Control.Monad.Memo.Class



newtype MemoT k v m a = MemoT { toStateT :: StateT (M.Map k v) m a }


runMemoT = runStateT . toStateT
startRunMemoT = (`runMemoT` M.empty)

type Memo k v = MemoT k v Identity

runMemo m = runIdentity . runMemoT m
startRunMemo = (`runMemo`M.empty)

evalMemoT m s = runMemoT m s >>= return . fst
startEvalMemoT = (`evalMemoT` M.empty)

evalMemo m = runIdentity . evalMemoT m
startEvalMemo = (`evalMemo`M.empty)




instance (Functor m) => Functor (MemoT k v m) where
    fmap f m = MemoT $ fmap f (toStateT m)

instance (Functor m, Monad m) => Applicative (MemoT k v m) where
    pure  = return 
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (MemoT k v m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (MemoT k v m) where
    return = MemoT . return
    m >>= k = MemoT $ (toStateT m) >>= (toStateT . k) 
    m >> n = MemoT $ (toStateT m) >> (toStateT n) 

instance (MonadPlus m) => MonadPlus (MemoT k v m) where
    mzero       = MemoT mzero
    m `mplus` n = MemoT $ toStateT m `mplus` toStateT n

instance (MonadFix m) => MonadFix (MemoT k v m) where
    mfix f = MemoT $ mfix (toStateT . f)


instance (Monad m, Ord k) => MonadCache k v (MemoT k v m) where
    lookup k = MemoT $ get >>= return . M.lookup k
    add k v  = MemoT $ modify $ \m -> M.insert k v m

instance (Monad m, Ord k) => MonadMemo k v (MemoT k v m) where
    memo = memol0


instance (MonadIO m) => MonadIO (MemoT k v m) where
    liftIO = lift . liftIO

instance MonadTrans (MemoT k v) where
    lift = MemoT . lift