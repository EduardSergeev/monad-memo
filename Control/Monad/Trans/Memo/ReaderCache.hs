{- |
Module      :  Control.Monad.Trans.Memo.ReaderCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

Generic StateCache - wrapper around `Control.Monad.Trans.Reader.ReaderT`

-}

{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Memo.ReaderCache
(

  ReaderCache,
  evalReaderCache,
  container

) where

import Data.Function
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader


newtype ReaderCache c m a = ReaderCache { toReaderT :: ReaderT c m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

{-# INLINE evalReaderCache #-}
evalReaderCache = runReaderT . toReaderT

-- | Returns internal container
container :: Monad m => ReaderCache c m c
{-# INLINE container #-}
container = ReaderCache ask
