{- |
Module      :  Control.Monad.Trans.Memo.StateCache
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, flexible instances)

Generic StateCache - wrapper around `Control.Monad.Trans.State.Strict.StateT`

-}

{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Memo.StateCache
( 
    StateCache,
    runStateCache,
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
import Control.Monad.Trans.State.Strict


newtype StateCache c m a = StateCache { toStateT :: StateT c m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadTrans, MonadIO)

{-# INLINE runStateCache #-}
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
