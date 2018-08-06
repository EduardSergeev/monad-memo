{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Main (main) where

import Data.Int
import Data.Word
import Data.List
import qualified Data.IntMap as IM
import Data.Array
import Control.Monad.ST
import Control.Monad.Memo
import Control.Monad.Memo.Vector.Unsafe
import Control.Monad.Memo.Vector.Expandable
import Criterion.Main


-- Fibonacci numbers
--------------------

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


-- 0-1 Knapsack problem
-----------------------

{-# INLINE knap #-}
knap ws vs w = m (l-1) w
    where
      l = length ws
      wa = listArray (0,l-1) ws
      va = listArray (0,l-1) vs
      {-# INLINE m #-}
      m 0 _ = return 0
      m !i !w
          | wa ! i > w = for2 memo m (i-1) w
          | otherwise = do
        !m1 <- for2 memo m (i-1) w
        !m2 <- for2 memo m (i-1) (w - wa ! i)
        return (m1 `max` (m2 + va ! i))

knapM :: [Int] -> [Int] -> Int -> Int
knapM ws vs w = startEvalMemo (knap ws vs w)

knapSTA :: [Int] -> [Int] -> Int -> Int
knapSTA ws vs w = runST $ evalArrayMemo (knap ws vs w) ((0,0), ((length ws),w))

knapSTUA :: [Int] -> [Int] -> Int -> Int
knapSTUA ws vs w = runST $ evalUArrayMemo (knap ws vs w) ((0,0), ((length ws),w))

knapIOA :: [Int] -> [Int] -> Int -> IO Int
knapIOA ws vs w = evalArrayMemo (knap ws vs w) ((0,0), ((length ws),w))

knapIOUA :: [Int] -> [Int] -> Int -> IO Int
knapIOUA ws vs w = evalUArrayMemo (knap ws vs w) ((0,0), ((length ws),w))


-- Longest common subsequence
-----------------------------

{-# INLINE lcsm2 #-}
lcsm2 :: MonadMemo (Int,Int) Int m => [Int] -> [Int] -> m Int
lcsm2 as bs = lcs la lb
    where
      la = length as
      lb = length bs
      aa = listArray (1,la) as
      ba = listArray (1,lb) bs
      {-# INLINE lcs #-}
      lcs 0 _ = return 0
      lcs _ 0 = return 0
      lcs ia ib
          | (aa!ia) == (ba!ib) = succ `liftM` for2 memo lcs (ia-1) (ib-1)
          | otherwise = do
        !l1 <- for2 memo lcs (ia-1) ib
        !l2 <- for2 memo lcs ia (ib-1)
        return (l1 `max` l2)

lcsM :: [Int] -> [Int] -> Int
lcsM as bs = startEvalMemo (lcsm2 as bs)

lcsSTA :: [Int] -> [Int] -> Int
lcsSTA as bs = runST $ evalArrayMemo (lcsm2 as bs) ((0,0), (length as, length bs))

lcsSTUA :: [Int] -> [Int] -> Int
lcsSTUA as bs = runST $ evalUArrayMemo (lcsm2 as bs) ((0,0), (length as, length bs))

{-# INLINE lcsm #-}
lcsm :: MonadMemo Int Int m => [Int] -> [Int] -> m Int
lcsm as bs = lcs la lb
    where
      la = genericLength as
      lb = genericLength bs
      aa = listArray (1,la) as
      ba = listArray (1,lb) bs
      {-# INLINE lcs #-}
      lcs 0 _ = return 0
      lcs _ 0 = return 0
      lcs ia ib
          | (aa!ia) == (ba!ib) = succ `liftM` mlcs (ia-1) (ib-1)
          | otherwise = do
        l1 <- mlcs (ia-1) ib
        l2 <- mlcs ia (ib-1)
        return (l1 `max` l2)
      mlcs ai bi =
          memo (\abi -> 
                    let (!ai,!bi) = abi `quotRem` lb
                    in lcs ai bi) (ai*lb + bi)

lcsIM :: [Int] -> [Int] -> Int
lcsIM as bs = evalMemoState (lcsm as bs) IM.empty

lcsSTUV :: [Int] -> [Int] -> Int
lcsSTUV as bs = runST $ evalUVectorMemo (lcsm as bs) ((length as + 1) * (length bs + 1))

lcsSTUVE :: [Int] -> [Int] -> Int
lcsSTUVE as bs = runST $ startEvalUVectorMemo (lcsm as bs)



main = defaultMainWith defaultConfig [
         bgroup "fib" [
           bgroup "pure" [
             bench "Map" $ whnf fibM n
           , bench "IntMap" $ whnf fibIM n
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
             bench "Array" $ whnfIO (fibIOA n)
           , bench "UArray" $ whnfIO (fibIOUA n)
           , bench "Vector" $ whnfIO (fibIOV n)
           , bench "UVector" $ whnfIO (fibIOUV n)
           , bench "Vector unsafe" $ whnfIO (fibIOVU n)
           , bench "UVector unsafe" $ whnfIO (fibIOUVU n)
           , bench "Vector exp" $ whnfIO (fibIOVE n)
           , bench "UVector exp" $ whnfIO (fibIOUVE n)
           ]
         ]
       , bgroup "knapsack" [
          bgroup "pure" [
             bench "Map" $ whnf (knapM ws vs) w
          ]
        , bgroup "ST" [
             bench "Array" $ whnf (knapSTA ws vs) w
           , bench "UArray" $ whnf (knapSTUA ws vs) w
          ]
        , bgroup "IO" [
             bench "Array" $ whnfIO (knapIOA ws vs w)
           , bench "UArray" $ whnfIO (knapIOUA ws vs w)
          ]
         ]
       , bgroup "LCS" [
          bgroup "pure" [
             bench "Map" $ whnf (lcsM as) bs
           , bench "IntMap" $ whnf (lcsIM as) bs
          ]
        , bgroup "ST" [
             bench "Array" $ whnf (lcsSTA as) bs
           , bench "UArray" $ whnf (lcsSTUA as) bs
           , bench "UVector exp" $ whnf (lcsSTUVE as) bs
           , bench "UVector" $ whnf (lcsSTUV as) bs
         ]
        ]
       ]
    where
      -- fib arg
      n = 100000
      -- knapsac args
      ws = [1..200]
      vs = [1..200]
      w = 800
      -- LCS args
      as = [1..400]
      bs = [100,102..800]
           
