{- |
Module      :  Data.MapLike
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines MapLike typeclass - generalized interface to Data.Map, Data.HashMap etc.

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}

module Data.MapLike (

   MapLike(..),

) where

import Data.Maybe

class MapLike m k v where
    lookup :: k -> m k v -> Maybe v
    add :: k -> v -> m k v -> m k v



