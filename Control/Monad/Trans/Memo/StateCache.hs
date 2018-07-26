{- |
Module      :  Control.Monad.Trans.Memo.StateCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Generic StateCache - similar to `Control.Monad.Trans.State.Strict.StateT` but optimised for carrying cache container

-}

{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module Control.Monad.Trans.Memo.StateCache
(
 
    StateCache(..),
    container,
    setContainer,
    evalStateCache

) where

import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.Class

-- | Generic memoization cache which uses provided container which can also be updated by the computation.
-- This is pretty much identical to `Control.Monad.Trans.State.Strict.StateT`,
-- but is tuned to speed up implementations which use unboxed mutable containers
newtype StateCache c m a = StateCache { runStateCache :: c -> m (a, c) }

-- | Evaluates computation discarding the resulting container 
evalStateCache :: Monad m => StateCache c m a -> c -> m a
{-# INLINE evalStateCache #-}
evalStateCache m !c = do
    (a, _) <- runStateCache m c
    return a

-- | Returns internal container
container :: Monad m => StateCache c m c
{-# INLINE container #-}
container = StateCache $ \ !c -> return (c, c)

-- | Assigns new value to internal container
setContainer :: Monad m => c -> StateCache c m ()
{-# INLINE setContainer #-}
setContainer c = StateCache $ \_ -> return ((), c)


instance (Functor m) => Functor (StateCache c m) where
    {-# INLINE fmap #-}
    fmap f m = StateCache $ \ !c ->
        fmap (\ (a, c') -> (f a, c')) (runStateCache m c)

instance (Functor m, Monad m) => Applicative (StateCache c m) where
    {-# INLINE pure #-}
    pure = return
    {-# INLINE (<*>) #-}
    fa <*> aa = StateCache $ \ !c -> do
        (f, c') <- runStateCache fa c
        (a, c'') <- runStateCache aa c'
        return (f a, c'')
       
instance (Functor m, MonadPlus m) => Alternative (StateCache c m) where
    {-# INLINE empty #-}
    empty = mzero
    {-# INLINE (<|>) #-}
    (<|>) = mplus

instance (Monad m) => Monad (StateCache c m) where
    {-# INLINE return #-}
    return a = StateCache $ \ !c -> return (a, c)
    {-# INLINE (>>=) #-}
    m >>= k  = StateCache $ \ !c -> do
        (a, !c') <- runStateCache m c
        runStateCache (k a) c'
    {-# INLINE (>>) #-}
    m >> n   = StateCache $ \ !c -> do
        (_, !c') <- runStateCache m c
        runStateCache n c'         
    fail str = StateCache $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateCache c m) where
    {-# INLINE mzero #-}
    mzero       = StateCache $ const mzero
    {-# INLINE mplus #-}
    m `mplus` n = StateCache $ \ !c -> runStateCache m c `mplus` runStateCache n c

instance (MonadFix m) => MonadFix (StateCache c m) where
    mfix f = StateCache $ \ !c -> mfix $ \ ~(a, _) -> runStateCache (f a) c

instance MonadTrans (StateCache c) where
    {-# INLINE lift #-}
    lift m = StateCache $ \ !c -> do
        a <- m
        return (a, c)

instance (MonadIO m) => MonadIO (StateCache c m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO
