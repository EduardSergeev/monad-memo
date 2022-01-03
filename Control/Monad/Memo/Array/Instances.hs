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
  UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

module Control.Monad.Memo.Array.Instances
(

   module Control.Monad.Memo.Array

) where

import Data.Maybe
import Data.MaybeLike
import Control.Monad.Memo.Array


instance MaybeLike (Maybe v) v => ArrayMemo v (Maybe v)

instance MaybeLike v v => UArrayMemo v v
