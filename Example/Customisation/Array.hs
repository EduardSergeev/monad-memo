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
  FlexibleContexts, UndecidableInstances #-}

module Example.Customisation.Array
(

  -- * Custom `ArrayMemo`
  -- $UnboxedInt16TupleArray
  Int16Sum,
  evalFibSTUA,
  runFibSTUA,
  evalFibIOUA,
  runFibIOUA

) where

import Data.Ix
import Data.Int
import Data.Array.MArray (MArray)
import Data.Array.Unsafe (unsafeFreeze)
import qualified Data.Array.Unboxed as UA
import Control.Monad.ST
import Control.Monad.Writer

import Data.MaybeLike
import Control.Monad.Memo.Class
import Control.Monad.Memo.Mutable.Array


fibmw 0 = return 0
fibmw 1 = return 1
fibmw n = do
  f1 <- memo fibmw (n-1)
  f2 <- memo fibmw (n-2)
  tell $ Sum 1
  return (f1+f2)

{- $UnboxedInt16TupleArray
The way to memoize a tuple of Int16 values using unboxed `UArrayCache`
-}

-- | A tuple of unboxed `Int16` and `Sum` of it
type Int16Sum = (Int16,Sum Int16)

-- | `MaybeLike` instance for our tuple
instance MaybeLike Int32 Int16Sum where
    nothing = minBound
    isNothing v = v == minBound
    just (a,Sum b) = (fromIntegral a)*2^16 + (fromIntegral b)
    fromJust v =
        let (a,b) = v `divMod` (2^16)
        in (fromIntegral a, Sum (fromIntegral b))

-- | `UArrayMemo` instance for our tuple
-- Now we can use `evalUArrayMemo` and `runUArrayMemo` methods
instance (Monad m, Ix k, MArray (UArray m) Int32 m) => UArrayMemo k Int16Sum Int32 m 


evalFibSTUA :: Int -> Int16Sum
evalFibSTUA n = runST $ evalUArrayMemo (runWriterT (fibmw n)) (0,n)

runFibSTUA :: Int -> (Int16Sum, UA.UArray Int Int32)
runFibSTUA n = runST $ do 
   (a,arr) <- runUArrayMemo (runWriterT (fibmw n)) (0,n)
   iarr <- unsafeFreeze arr
   return (a, iarr)


evalFibIOUA :: Int -> IO Int16Sum
evalFibIOUA n = (`evalUArrayMemo`(0,n)) . runWriterT . fibmw $ n 

runFibIOUA :: Int -> IO (Int16Sum, UA.UArray Int Int32)
runFibIOUA n = do
   (a,arr) <- runUArrayMemo (runWriterT (fibmw n)) (0,n)
   iarr <- unsafeFreeze arr
   return (a, iarr)

