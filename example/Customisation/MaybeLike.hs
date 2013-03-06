{- |
Module      :  Sample.Memo
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

More advanced examples

-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, UndecidableInstances  #-}

module Example.Customisation.MaybeLike
(

  -- * Customised `MaybeLike`
  -- $MaybeLike
  runFibSTUA,

) where

import Data.Array.MArray
import qualified Data.Array.Unboxed as UA
import Control.Monad
import Control.Monad.ST

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Memo.Array.Instances


fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n-1)
  f2 <- memo fibm (n-2)
  return (f1+f2)


{- $MaybeLike
Default implementation of `ArrayCache` and `VectorCache` uses `minBound` for `Bounded` types and `NaN` for `RealFloat` types as "null" value (i.e. missing result in memo-cache). However it is possible to override these default settings. To do that we have to exclude default definitions from "Control.Monad.Memo" (and manualy import all relevant modules like in this example). Then we just need to implement `MaybeLike` instance for our type after which we can use all existing methods of running `ArrayCache` or `VectorCache`. 
-}

-- | Our customised version of `MaybeLike` for Double with @`nothing` == (-1)@
-- to be used with any unboxed `ArrayCache`  
instance MaybeLike Double Double where
    nothing = -1
    isNothing = (<0)
    just v = v
    fromJust v = v 

evalFibSTUA :: Int -> Double
evalFibSTUA n = runST $ evalUArrayMemo (fibm n) (0,n)

-- | This also produces resulting array
runFibSTUA :: Int -> (Double, UA.UArray Int Double)
runFibSTUA n = runST $ do
    (a,arr) <- runUArrayMemo (fibm n) (0,n)
    iarr <- freeze arr
    return (a,iarr)