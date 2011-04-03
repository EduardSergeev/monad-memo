{- |
Module      :  Control.Monad.Trans.Memo.Map
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

MemoT as a specialization of MemoStateT with Data.Map as a container

-}

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
import Data.MapLike.Instances

import Control.Monad.Identity
import Control.Monad.Trans.Memo.State

import qualified Data.Map as M


type MemoT k v = MemoStateT (M.Map k v) k v

type Memo k v = MemoT k v Identity


runMemoT :: MemoT k v m a -> M.Map k v -> m (a, M.Map k v)
runMemoT = runMemoStateT

evalMemoT :: Monad m => MemoT k v m a -> M.Map k v -> m a
evalMemoT = evalMemoStateT

runMemo :: Memo k v a -> M.Map k v -> (a, M.Map k v)
runMemo = runMemoState

evalMemo :: Memo k v a -> M.Map k v -> a
evalMemo = evalMemoState



startRunMemoT :: Monad m => MemoT k v m a -> m (a, M.Map k v)
startRunMemoT = (`runMemoT` M.empty)

startEvalMemoT :: Monad m => MemoT k v m a -> m a
startEvalMemoT = (`evalMemoT` M.empty)

startRunMemo :: Memo k v a -> (a, M.Map k v)
startRunMemo = (`runMemo` M.empty)

startEvalMemo :: Memo k v a -> a
startEvalMemo = (`evalMemo` M.empty)
