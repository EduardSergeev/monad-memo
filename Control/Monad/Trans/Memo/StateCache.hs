{- |
Module      :  Control.Monad.Trans.Memo.StateCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Generic StateCache - wrapper around `Control.Monad.Trans.State.Strict.StateT`

-}

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Control.Monad.Trans.Memo.StateCache
( 
    StateCache,
    runStateCache,
    container,
    setContainer,
    evalStateCache
) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Array.MArray
import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Array.Base
import Data.Array.IO
import Data.Array.ST



newtype StateCache c m a = StateCache { toStateT :: StateT c m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

{-# INLINE runStateCache #-}
runStateCache :: StateCache s m a -> s -> m (a, s)
runStateCache = runStateT . toStateT

-- | Evaluates computation discarding the resulting container 
evalStateCache :: Monad m => StateCache c m a -> c -> m a
{-# INLINE evalStateCache #-}
evalStateCache = evalStateT . toStateT

-- | Returns internal container
container :: Monad m => StateCache c m c
{-# INLINE container #-}
container = StateCache get

-- | Assigns new value to internal container
setContainer :: Monad m => c -> StateCache c m ()
{-# INLINE setContainer #-}
setContainer = StateCache . put


instance PrimMonad m => PrimMonad (StateCache c m) where
  type PrimState (StateCache c m) = PrimState m
  primitive = lift . primitive


instance MArray IOArray e (StateCache c IO) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i

instance MArray IOUArray e IO => MArray IOUArray e (StateCache c IO) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i


instance MArray (STArray s) e (StateCache c (ST s)) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i

instance MArray (STUArray s) e (ST s) => MArray (STUArray s) e (StateCache c (ST s)) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i
