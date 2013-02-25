{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Main (main) where

import Data.Word
import qualified Data.IntMap as IM
import Control.Monad.ST
import Control.Monad.Memo
import Control.Monad.Memo.Vector.Unsafe
import Control.Monad.Memo.Vector.Expandable
import Criterion.Main
import Criterion.Config

n = 50000

main = defaultMainWith defaultConfig (return ()) [
         bgroup "fib" [
           bgroup "pure" [
             bench "Memo" $ whnf fibM n
           , bench "IntMap State" $ whnf fibIM n
           ]
         , bgroup "ST" [
             bench "Array" $ whnf fibSTA n
           , bench "UArray" $ whnf fibSTUA n
           , bench "Vector" $ whnf fibSTV n
           , bench "UVector" $ whnf fibSTUV n
           , bench "Vector unsafe" $ whnf fibSTVU n
           , bench "UVector unsafe" $ whnf fibSTUVU n
           , bench "Vector exp" $ whnf fibSTVE n
           , bench "UVector exp" $ whnf fibSTUVE n
           ]
         , bgroup "IO" [
             bench "Array" $ fibIOA n
           , bench "UArray" $ fibIOUA n
           , bench "Vector" $ fibIOV n
           , bench "UVector" $ fibIOUV n
           , bench "Vector unsafe" $ fibIOVU n
           , bench "UVector unsafe" $ fibIOUVU n
           , bench "Vector exp" $ fibIOVE n
           , bench "UVector exp" $ fibIOUVE n
           ]
         ]
       ]
        
{-# INLINE fibm #-}
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n - 1)
  f2 <- memo fibm (n - 2)
  return (f1+f2)

fibM :: Int -> Word
fibM = startEvalMemo . fibm

fibIM :: Int -> Word
fibIM n = evalMemoState (fibm n) IM.empty

fibIOA :: Int -> IO Word
fibIOA n = evalArrayMemo (fibm n) (0,n)

fibIOUA :: Int -> IO Word
fibIOUA n = evalUArrayMemo (fibm n) (0,n)

fibSTA :: Int -> Word
fibSTA n = runST $ evalArrayMemo (fibm n) (0,n)

fibSTUA :: Int -> Word
fibSTUA n = runST $ evalUArrayMemo (fibm n) (0,n)


fibIOV :: Int -> IO Word
fibIOV n = evalVectorMemo (fibm n) n

fibIOUV :: Int -> IO Word
fibIOUV n = evalUVectorMemo (fibm n) n

fibSTV :: Int -> Word
fibSTV n = runST $ evalVectorMemo (fibm n) n

fibSTUV :: Int -> Word
fibSTUV n = runST $ evalUVectorMemo (fibm n) n


fibIOVU :: Int -> IO Word
fibIOVU n = unsafeEvalVectorMemo (fibm n) n

fibIOUVU :: Int -> IO Word
fibIOUVU n = unsafeEvalUVectorMemo (fibm n) n

fibSTVU :: Int -> Word
fibSTVU n = runST $ unsafeEvalVectorMemo (fibm n) n

fibSTUVU :: Int -> Word
fibSTUVU n = runST $ unsafeEvalUVectorMemo (fibm n) n


fibIOVE :: Int -> IO Word
fibIOVE n = startEvalVectorMemo (fibm n)

fibIOUVE :: Int -> IO Word
fibIOUVE n = startEvalUVectorMemo (fibm n)

fibSTVE :: Int -> Word
fibSTVE n = runST $ startEvalVectorMemo (fibm n)

fibSTUVE :: Int -> Word
fibSTUVE n = runST $ startEvalUVectorMemo (fibm n)
