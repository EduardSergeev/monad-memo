{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction,
MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Trans.Memo.Custom
(
 
MemoT(..),
startRunMemoT,
evalMemoT,
startEvalMemoT,

Memo(),
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
import Control.Monad.Identity
import Control.Monad.Trans.Class
import qualified Data.Map as M 

import Control.Monad.Memo.Class


newtype MemoT k v m a = MemoT { runMemoT :: M.Map k v -> m (a , M.Map k v) }


startRunMemoT = (`runMemoT` M.empty)

type Memo k v = MemoT k v Identity

runMemo m = runIdentity . runMemoT m
startRunMemo = (`runMemo`M.empty)

evalMemoT m s = runMemoT m s >>= return . fst
startEvalMemoT = (`evalMemoT` M.empty)

evalMemo m = runIdentity . evalMemoT m
startEvalMemo = (`evalMemo`M.empty)



instance (Functor m) => Functor (MemoT k v m) where
    fmap f m = MemoT $ \ s ->
        fmap (\(a, s') -> (f a, s')) $ runMemoT m s


instance (Monad m) => Monad (MemoT k v m) where
    return a = MemoT $ \s -> return (a, s)
    m >>= k  = MemoT $ \s -> do
        (a, s') <- runMemoT m s
        runMemoT (k a) s'
    fail str = MemoT $ \_ -> fail str


instance (Monad m, Ord k) => MonadMemo k v (MemoT k v m) where
    lookup k = MemoT $ \m -> return (M.lookup k m , m)
    add k v = MemoT $ \m -> return (() , M.insert k v m)

instance MonadTrans (MemoT k v) where
    lift m = MemoT $ \s -> do
        a <- m
        return (a, s)


