{- |
Module      :  Control.Monad.Trans.Memo.ReaderCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

Generic StateCache - similar to `Control.Monad.Trans.Reader.ReaderT` but optimised for carrying cache container

-}

{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module Control.Monad.Trans.Memo.ReaderCache
(

  ReaderCache(..),
  container

) where

import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.Class


-- | Generic memoization cache which uses provided container
-- This is pretty much identical to `Control.Monad.Trans.Reader.ReaderT`,
-- but is tuned to speed up implementations which use unboxed mutable containers
newtype ReaderCache c m a = ReaderCache { evalReaderCache :: c -> m a }

-- | Returns internal container
container :: Monad m => ReaderCache c m c
{-# INLINE container #-}
container = ReaderCache $ \ !c -> return c


instance (Functor m) => Functor (ReaderCache c m) where
    {-# INLINE fmap #-}
    fmap f m = ReaderCache $ \ !c -> fmap f (evalReaderCache m c)

instance (Applicative m) => Applicative (ReaderCache arr m) where
    {-# INLINE pure #-}
    pure a  = ReaderCache $ \_ -> pure a
    {-# INLINE (<*>) #-}
    f <*> v = ReaderCache $ \ !c -> evalReaderCache f c <*> evalReaderCache v c

instance (Alternative m) => Alternative (ReaderCache c m) where
    {-# INLINE empty #-}
    empty   = ReaderCache $ const empty
    {-# INLINE (<|>) #-}
    m <|> n = ReaderCache $ \ !c -> evalReaderCache m c <|> evalReaderCache n c

instance (Monad m) => Monad (ReaderCache c m) where
    {-# INLINE return #-}
    return a = ReaderCache $ \ !c -> return a
    {-# INLINE (>>=) #-}
    m >>= k  = ReaderCache $ \ !c -> do
        a <- evalReaderCache m c
        evalReaderCache (k a) c
    {-# INLINE (>>) #-}
    m >> k   = ReaderCache $ \ !c -> do
        evalReaderCache m c
        evalReaderCache k c

instance (MonadPlus m) => MonadPlus (ReaderCache c m) where
    {-# INLINE mzero #-}
    mzero       = lift mzero
    {-# INLINE mplus #-}
    m `mplus` n = ReaderCache $ \ !c -> evalReaderCache m c `mplus` evalReaderCache n c

instance (MonadFix m) => MonadFix (ReaderCache c m) where
    mfix f = ReaderCache $ \ !c -> mfix $ \a -> evalReaderCache (f a) c

instance MonadTrans (ReaderCache c) where
    {-# INLINE lift #-}
    lift = ReaderCache . const

instance (MonadIO m) => MonadIO (ReaderCache c m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO