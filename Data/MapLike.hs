{- |
Module      :  Data.MapLike
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Defines MapLike typeclass - generalized interface to Data.Map, Data.HashMap etc.

-}

{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FunctionalDependencies #-}

module Data.MapLike (

   MapLike(..),

) where

import Data.Maybe


--class MapLike m k where
--    lookup :: k -> m k v -> Maybe v
--    add :: k -> v -> m k v -> m k v

class MapLike c k v | c -> k, c -> v where
    lookup :: k -> c -> Maybe v
    add :: k -> v -> c -> c



