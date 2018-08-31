{- |
Module      :  Control.Monad.Trans.Memo.ReaderCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

Generic StateCache - wrapper around `Control.Monad.Trans.Reader.ReaderT`

-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts,
    TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.Trans.Memo.ReaderCache
(

  ReaderCache,
  evalReaderCache,
  container

) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Array.Base
import Data.Array.IO
import Data.Array.ST

newtype ReaderCache c m a = ReaderCache { toReaderT :: ReaderT c m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

{-# INLINE evalReaderCache #-}
evalReaderCache :: ReaderCache r m a -> r -> m a
evalReaderCache = runReaderT . toReaderT

-- | Returns internal container
container :: Monad m => ReaderCache c m c
{-# INLINE container #-}
container = ReaderCache ask


instance PrimMonad m => PrimMonad (ReaderCache c m) where
  type PrimState (ReaderCache c m) = PrimState m
  primitive = lift . primitive


instance MArray IOArray e (ReaderCache c IO) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i

instance MArray IOUArray e IO => MArray IOUArray e (ReaderCache c IO) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i


instance MArray (STArray s) e (ReaderCache c (ST s)) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i

instance MArray (STUArray s) e (ST s) => MArray (STUArray s) e (ReaderCache c (ST s)) where
  getBounds = lift . getBounds
  getNumElements = lift . getNumElements
  newArray a = lift . newArray a
  unsafeRead a = lift . unsafeRead a
  unsafeWrite a i = lift . unsafeWrite a i
