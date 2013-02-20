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
  UndecidableInstances #-}

module Control.Monad.Trans.Memo.StateCache
(
 
    StateCache(..),
    container,
    setContainer,
    update,
    evalStateCache

) where


import Data.Function
import Data.Tuple
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Identity


newtype StateCache c m a = StateCache { runStateCache :: c -> m (a, c) }

evalStateCache :: Monad m => StateCache c m a -> c -> m a
{-# INLINE evalStateCache #-}
evalStateCache m c = do
    (a, _) <- runStateCache m c
    return a

container :: Monad m => StateCache c m c
{-# INLINE container #-}
container = StateCache $ \c -> return (c, c)

setContainer :: Monad m => c -> StateCache c m ()
{-# INLINE setContainer #-}
setContainer c = StateCache $ \_ -> return ((), c)

update :: Monad m => (c -> m (a, c)) -> StateCache c m a
{-# INLINE update #-}
update f = StateCache $ f


instance (Functor m) => Functor (StateCache c m) where
    {-# INLINE fmap #-}
    fmap f m = StateCache $ \c ->
        fmap (\ (a, c') -> (f a, c')) (runStateCache m c)

instance (Functor m, Monad m) => Applicative (StateCache c m) where
    {-# INLINE pure #-}
    pure = return
    {-# INLINE (<*>) #-}
    fa <*> aa = StateCache $ \c -> do
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
    return a = StateCache $ \c -> return (a, c)
    {-# INLINE (>>=) #-}
    m >>= k  = StateCache $ \c -> do
        (a, c') <- runStateCache m c
        runStateCache (k a) c'
    {-# INLINE (>>) #-}
    m >> n   = StateCache $ \c -> do
        (_, c') <- runStateCache m c
        runStateCache n c'         
    fail str = StateCache $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateCache c m) where
    {-# INLINE mzero #-}
    mzero       = StateCache $ \_ -> mzero
    {-# INLINE mplus #-}
    m `mplus` n = StateCache $ \c -> runStateCache m c `mplus` runStateCache n c

instance (MonadFix m) => MonadFix (StateCache c m) where
    mfix f = StateCache $ \c -> mfix $ \ ~(a, _) -> runStateCache (f a) c

instance MonadTrans (StateCache c) where
    {-# INLINE lift #-}
    lift m = StateCache $ \c -> do
        a <- m
        return (a, c)

instance (MonadIO m) => MonadIO (StateCache c m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO
