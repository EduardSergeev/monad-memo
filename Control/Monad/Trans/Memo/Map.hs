{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Trans.Memo.Map (

   MemoT,
   runMemoT,
   evalMemoT,
   startRunMemoT,
   startEvalMemoT,

   Memo,
   runMemo,
   evalMemo,
   startRunMemo,
   startEvalMemo,

) where

import Data.Ord
import qualified Data.Map as M
import qualified Data.MapLike as Ml
import Control.Monad.Identity
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.State

instance Ord k => Ml.MapLike M.Map k v where
    add = M.insert
    lookup = M.lookup

type MemoT = MemoStateT M.Map

type Memo k v = MemoT k v Identity


runMemoT :: MemoT k v m a -> M.Map k v -> m (a, M.Map k v)
runMemoT = runMemoStateT

evalMemoT :: (Monad m) => MemoT k v m a -> M.Map k v -> m a
evalMemoT = evalMemoStateT

runMemo :: Memo k v a -> M.Map k v -> (a, M.Map k v)
runMemo = runMemoState

evalMemo :: Memo k v a -> M.Map k v -> a
evalMemo = evalMemoState



startRunMemoT :: (Monad m) => MemoT k v m a -> m (a, M.Map k v)
startRunMemoT = (`runMemoT` M.empty)

startEvalMemoT :: (Monad m) => MemoT k v m a -> m a
startEvalMemoT = (`evalMemoT` M.empty)

startRunMemo :: Memo k v a -> (a, M.Map k v)
startRunMemo = (`runMemo` M.empty)

startEvalMemo :: Memo k v a -> a
startEvalMemo = (`evalMemo` M.empty)
