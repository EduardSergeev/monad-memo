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
  UndecidableInstances, FlexibleInstances, RankNTypes #-}


module Control.Monad.Memo.Class
(

      MonadCache(..),
      MonadMemo(..),

      memoln,
      memol0,
      memol1,
      memol2,
      memol3,
      memol4

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
import qualified Control.Monad.Trans.State.Lazy as Lazy -- (StateT, get, put)
import qualified Control.Monad.Trans.State.Strict as Strict -- (StateT, get, put)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict


class Monad m => MonadCache k v m | m -> k, m -> v where
    lookup :: k -> m (Maybe v)
    add :: k -> v -> m ()

class Monad m => MonadMemo k v m | m -> k, m -> v where
    memo :: (k -> m v) -> k -> m v


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

instance (MonadMemo k [v] m) => MonadMemo k v (ListT m) where
    memo f = ListT . memo (runListT . f)

instance (Error e, MonadCache k  (Either e v) m) => MonadMemo k v (ErrorT e m) where
    memo f = ErrorT . memol0 (runErrorT . f)

instance (MonadCache (r,k) v m) => MonadMemo k v (ReaderT r m) where
    memo f k = do
      e <- ask
      memoln lift (e,) f k

instance (Monoid w, MonadCache k (v,w) m) => MonadMemo k v (Lazy.WriterT w m) where
    memo f = Lazy.WriterT . memol0 (Lazy.runWriterT . f)

instance (Monoid w, MonadCache k (v,w) m) => MonadMemo k v (Strict.WriterT w m) where
    memo f = Strict.WriterT . memol0 (Strict.runWriterT . f)


instance (MonadCache (s,k) v m) => MonadMemo k v (Lazy.StateT s m) where
    memo f k = do
      s <- Lazy.get
      memoln lift (s,) f k

instance (MonadCache (s,k) v m) => MonadMemo k v (Strict.StateT s m) where
    memo f k = do
      s <- Strict.get
      memoln lift (s,) f k
