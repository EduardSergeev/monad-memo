{- |
Module      :  Control.Monad.Trans.Memo.Array.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Default instances of `ArrayMemo` and `UArrayMemo`

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  UndecidableInstances, FlexibleInstances #-}

module Control.Monad.Memo.Mutable.Array.Instances
(

   module Control.Monad.Memo.Mutable.Array

) where

import Data.Ix
import Data.Maybe
import Data.Array.MArray
import Control.Monad

import Data.MaybeLike
import Control.Monad.Memo.Mutable
import Control.Monad.Memo.Mutable.Array


instance (Monad m, Ix k, MaybeLike (Maybe v) v,
          MArray (Array m) (Maybe v) m,
          Mutable k (Maybe v) (Array m k (Maybe v)) m) =>
    ArrayMemo k v (Maybe v) m


instance (Monad m, Ix k, MaybeLike v v,
          MArray (UArray m) v m,
          Mutable k v (UArray m k v) m) =>
    UArrayMemo k v v m 