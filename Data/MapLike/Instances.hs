{- |
Module      :  Data.MapLike.Instances
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines MapLike instances declaration for standard data types

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}

module Data.MapLike.Instances (

   MapLike(..)

) where

import Data.Ord
import Data.Int
import Data.MapLike   
import qualified Data.Map as M
import qualified Data.IntMap as IM


-- | Data.Map is a default implementation (not the fastest but well-known)
instance Ord k => MapLike (M.Map k v) k v where
    add = M.insert
    lookup = M.lookup

-- | Data.IntMap is usually more efficient that Data.Map if @k :: Int@ 
instance MapLike (IM.IntMap v) Int v where
    {-# INLINE add #-}
    add = IM.insert
    {-# INLINE lookup #-}
    lookup = IM.lookup
