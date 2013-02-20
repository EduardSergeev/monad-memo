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
  UndecidableInstances, FlexibleInstances, TypeFamilies #-}

module Control.Monad.Memo.Vector.Instances
(

  
) where

import Data.Maybe

import Data.MaybeLike
import qualified Control.Monad.Memo.Vector as V
import qualified Control.Monad.Memo.Vector.Expandable as E
import qualified Control.Monad.Memo.Vector.Unsafe as U


instance MaybeLike (Maybe v) v => V.VectorMemo v (Maybe v)

instance MaybeLike v v => V.UVectorMemo v v


instance MaybeLike (Maybe v) v => E.VectorMemo v (Maybe v)

instance MaybeLike v v => E.UVectorMemo v v


instance MaybeLike (Maybe v) v => U.VectorMemo v (Maybe v)

instance MaybeLike v v => U.UVectorMemo v v
