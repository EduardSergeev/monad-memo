{- |
Module      :  Control.Monad.Trans.Memo.Vector.Instances
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Default instances for `VectorMemo` and `UVectorMemo`

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses,
  UndecidableInstances, FlexibleInstances #-}

module Control.Monad.Memo.Mutable.Vector.Instances
(

   module Control.Monad.Memo.Mutable.Vector
  
) where

import Data.Maybe
import Data.Vector.Unboxed (Unbox)
import Control.Monad.Primitive

import Data.MaybeLike
import Control.Monad.Memo.Mutable
import Control.Monad.Memo.Mutable.Vector


instance (PrimMonad m, MaybeLike (Maybe v) v) => VectorMemo v (Maybe v) m

instance (PrimMonad m, Unbox v, MaybeLike v v) => UVectorMemo v v m