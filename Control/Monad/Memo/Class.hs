{- |
Module      :  Control.Monad.Memo.Class
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

[Computation type:] Interface for monadic computations which can be memoized.

-}

{-# LANGUAGE NoImplicitPrelude, TupleSections,
  MultiParamTypeClasses, FunctionalDependencies,
  UndecidableInstances, FlexibleInstances, FlexibleContexts, RankNTypes #-}


module Control.Monad.Memo.Class
(

      MonadCache(..),
      MonadMemo(..),

      for2,
      for3,
      for4,

      memoln,
      memol0,
      memol1,
      memol2,
      memol3,
      memol4,

) where

import Data.Function
import Data.Maybe
import Data.Either
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS


-- | Interface for memoization cache
-- Is necessary since memoization mechanism from one transformer can use a cache from other (further down the stack) 
class Monad m => MonadCache k v m | m -> k, m -> v where
    lookup :: k -> m (Maybe v)
    add :: k -> v -> m ()

-- | Memoization interface
class Monad m => MonadMemo k v m | m -> k, m -> v where
    memo :: (k -> m v) -> k -> m v

-- | Memoization for the current transformer in stack using a cache from an arbitrary transformer down the stack
memoln :: (MonadCache k2 v m1, Monad m1, Monad m2) =>
           (forall a.m1 a -> m2 a) -> (k1 -> k2)  -> (k1 -> m2 v) -> k1 -> m2 v
memoln fl fk f k = do
  mr <- fl $ lookup (fk k)
  case mr of
    Just r -> return r
    Nothing -> do
                r <- f k
                fl $ add (fk k) r
                return r

-- | Adapter for memoization of two-argument function
for2 :: (((k1, k2) -> mv) -> (k1, k2) -> mv) -> (k1 -> k2 -> mv) -> k1 -> k2 -> mv
for2 m f a b = m (\(a,b) -> f a b) (a,b)

-- | Adapter for memoization of three-argument function
for3 :: (((k1, k2, k3) -> mv) -> (k1, k2, k3) -> mv) -> (k1 -> k2 -> k3 -> mv) -> k1 -> k2 -> k3 -> mv
for3 m f a b c = m (\(a,b,c) -> f a b c) (a,b,c)


-- | Adapter for memoization of four-argument function
for4 :: (((k1, k2, k3, k4) -> mv) -> (k1, k2, k3, k4) -> mv) -> (k1 -> k2 -> k3 -> k4 -> mv) -> k1 -> k2 -> k3 -> k4 -> mv
for4 m f a b c d = m (\(a,b,c,d) -> f a b c d) (a,b,c,d)


-- | Uses current monad's memoization cache
memol0
    :: (MonadCache k v m, Monad m) =>
       (k -> m v) -> k -> m v
memol0 = memoln id id


-- | Uses the 1st transformer in stack for memoization cache
memol1
    :: (MonadTrans t1,
        MonadCache k v m,
        Monad (t1 m)) =>
       (k -> t1 m v) -> k -> t1 m v
memol1 = memoln lift id


-- | Uses the 2nd transformer in stack for memoization cache
memol2
  :: (MonadTrans t1,
      MonadTrans t2,
      MonadCache k v m,
      Monad (t2 m),
      Monad (t1 (t2 m))) =>
     (k -> t1 (t2 m) v) -> k -> t1 (t2 m) v
memol2 = memoln (lift . lift) id

-- | Uses the 3rd transformer in stack for memoization cache
memol3
  :: (MonadTrans t1,
      MonadTrans t2,
      MonadTrans t3,
      MonadCache k v m,
      Monad (t3 m),
      Monad (t2 (t3 m)),
      Monad (t1 (t2 (t3 m))) ) =>
     (k -> t1 (t2 (t3 m)) v) -> k -> t1 (t2 (t3 m)) v
memol3 = memoln (lift.lift.lift) id


-- | Uses the 4th transformer in stack for memoization cache
memol4
  :: (MonadTrans t1,
      MonadTrans t2,
      MonadTrans t3,
      MonadTrans t4,
      MonadCache k v m,
      Monad (t4 m),
      Monad (t3 (t4 m)),
      Monad (t2 (t3 (t4 m))),
      Monad (t1 (t2 (t3 (t4 m)))) ) =>
     (k -> t1 (t2 (t3 (t4 m))) v) -> k -> t1 (t2 (t3 (t4 m))) v
memol4 = memoln (lift.lift.lift.lift) id


instance (MonadCache k v m) => MonadMemo k v (IdentityT m) where
    memo f = IdentityT . memol0 (runIdentityT . f)

instance (MonadCache k v m) => MonadMemo k v (ContT r m) where
    memo = memol1

instance (MonadCache k (Maybe v) m) => MonadMemo k v (MaybeT m) where
    memo f = MaybeT . memol0 (runMaybeT . f)

instance (MonadCache k [v] m) => MonadMemo k v (ListT m) where
    memo f = ListT . memol0 (runListT . f)

instance (Error e, MonadCache k  (Either e v) m) => MonadMemo k v (ErrorT e m) where
    memo f = ErrorT . memol0 (runErrorT . f)

instance (MonadCache (r,k) v m) => MonadMemo k v (ReaderT r m) where
    memo f k = do
      r <- ask
      memoln lift (r,) f k

instance (Monoid w, MonadCache k (v,w) m) => MonadMemo k v (WL.WriterT w m) where
    memo f = WL.WriterT . memol0 (WL.runWriterT . f)

instance (Monoid w, MonadCache k (v,w) m) => MonadMemo k v (WS.WriterT w m) where
    memo f = WS.WriterT . memol0 (WS.runWriterT . f)


instance (MonadCache (s,k) v m) => MonadMemo k v (SL.StateT s m) where
    memo f k = do
      s <- SL.get
      memoln lift (s,) f k

instance (MonadCache (s,k) v m) => MonadMemo k v (SS.StateT s m) where
    memo f k = do
      s <- SS.get
      memoln lift (s,) f k


--instance (Monoid w, MonadCache (r,w,s,k) v m) => MonadMemo k v (RWSL.RWST r w s m) where
--    memo f k = do
--      r <- RWSL.ask
--      s <- RWSL.get
--      (_,w) <- RWSL.listen
--      memoln lift (r,s,w,) f k
