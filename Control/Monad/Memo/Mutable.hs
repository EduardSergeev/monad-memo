{- |
Module      :  Control.Monad.Memo.Mutable
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

-}

{-# LANGUAGE NoImplicitPrelude,
  MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, FlexibleContexts,
  UndecidableInstances, TypeFamilies #-}

module Control.Monad.Memo.Mutable
(

  Mutable(..),
  MutableCache(..)

) where

import Data.Function
import Data.Maybe (Maybe(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

import Data.MaybeLike
import Control.Monad.Memo.Class


-- | Memoization cache based on mutable container
newtype MutableCache c m a = MutableCache { evalMutableCache :: c -> m a }

instance (Functor m) => Functor (MutableCache c m) where
    {-# INLINE fmap #-}
    fmap f m = MutableCache $ \c -> fmap f (evalMutableCache m c)

instance (Applicative m) => Applicative (MutableCache arr m) where
    {-# INLINE pure #-}
    pure a   = MutableCache $ \_ -> pure a
    {-# INLINE (<*>) #-}
    f <*> v = MutableCache $ \ r -> evalMutableCache f r <*> evalMutableCache v r

instance (Monad m) => Monad (MutableCache c m) where
    {-# INLINE return #-}
    return a = MutableCache $ \_ -> return a
    {-# INLINE (>>=) #-}
    m >>= k  = MutableCache $ \c -> do
        a <- evalMutableCache m c
        evalMutableCache (k a) c
    {-# INLINE (>>) #-}
    m >> k   = m >>= \ _ -> k

instance (MonadFix m) => MonadFix (MutableCache c m) where
    mfix f = MutableCache $ \c -> mfix $ \a -> evalMutableCache (f a) c


class Monad m => Mutable k e c m | c -> e, c -> k where
    mutableRead :: c -> k -> m e
    mutableWrite :: c -> k -> e -> m ()


instance (MaybeLike e v, Mutable k e c m) => MonadCache k v (MutableCache c m) where
        {-# INLINE lookup #-}
        lookup k = MutableCache $ \c -> do
                     mv <- mutableRead c k
                     return $ if isNothing mv then Nothing else Just (fromJust mv)
        {-# INLINE add #-}
        add k v = MutableCache $ \c -> mutableWrite c k (just v)

instance (MaybeLike e v, Mutable k e c m) => MonadMemo k v (MutableCache c m) where
        {-# INLINE memo #-}
        memo f k = MutableCache $ \c -> do
          e <- mutableRead c k
          if isNothing e
             then do
               v <- evalMutableCache (f k) c
               mutableWrite c k (just v)
               return v
             else return (fromJust e)

