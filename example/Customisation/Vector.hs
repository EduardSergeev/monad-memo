{- |
Module      :  Sample.Memo
Copyright   :  (c) Eduard Sergeev 2013
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable

More advanced examples
-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
  TypeFamilies, TypeSynonymInstances #-}

module Example.Customisation.Vector
(

  -- * Custom `VectorMemo`
  -- $UnboxedTupleVector
  BoolInt,
  evalFibSTUV,
  runFibSTUV,
  evalFibIOUV,
  runFibIOUV

) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.ReaderCache
import Control.Monad.Memo.Vector


fibm 0 = return 0
fibm 1 = return 1
fibm n = liftM2 (+) (memo fibm (n-1)) (memo fibm (n-2))

{- $UnboxedTupleVector
New custom types, not handled by "Control.Monad.Trans.Memo.Vector.Instances", can be used inside `VectorCache` if necessary.
For example if we need a full range of `Int` (including `minBound` and `maxBound`) we can represent `Maybelike` `Int` as a pair @(Bool,Int)@ with `nothing` indicated by the @False@ value of its first element. `Data.Vector.Unboxed.Mutable.MVector` can store such pair efficiently (internally as a pair of unboxed arrays) so all we have to do then is to define `MaybeLike` and `VectorMemo` instances for our product-type.
-}

-- | Unboxed `Int` which can memoize entire range of `Int` values
-- by indicating `nothing` values by setting its first element to @False@
type BoolInt = (Bool,Int)

-- | MaybeLike instance for our unboxed Int
instance MaybeLike BoolInt Int where
    nothing = (False,0)
    isNothing (b,_) = not b
    just a = (True,a)
    fromJust (True,a) = a

-- | UVectorMemo instance will allow us to use all @eval*@ and @run*@ functions
-- from unboxed part of "Control.Monad.Trans.Memo.Vector" module 
instance UVectorMemo Int BoolInt


-- | Use standard function once we defined the instance for `VectorMemo`
evalFibSTUV :: Int -> Int
evalFibSTUV n = runST $ evalUVectorMemo (fibm n) (n+1)

runFibSTUV :: Int -> (Int, UV.Vector (Bool,Int))
runFibSTUV n = runST $ do 
   (a,vec) <- runUVectorMemo (fibm n) (n+1)
   ivec <- UV.unsafeFreeze vec
   return (a, ivec)

evalFibIOUV :: Int -> IO Int
evalFibIOUV n = evalUVectorMemo (fibm n) (n+1) 

runFibIOUV :: Int -> IO (Int, UV.Vector (Bool,Int))
runFibIOUV n = do
  (a, vec) <- runUVectorMemo (fibm n) (n+1) 
  ivec <- UV.unsafeFreeze vec
  return (a, ivec)
