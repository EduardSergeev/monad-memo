{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Memo.Test
(
       tests
) where

import Test.QuickCheck
import System.Random

import Control.Monad.Memo
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.List

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)



newtype SmallInt n = SmallInt { toInt::n } deriving Show

instance (Num n, Random n) => Arbitrary (SmallInt n) where
    arbitrary = fmap SmallInt $ choose (0,10)

newtype SmallList a = SmallList { toList::[a] } deriving Show

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = do
      n <- choose (0,10)
      ls <- arbitrary
      return $ SmallList $ take n ls 



-- | With ReaderT
fibr 0 = return 0
fibr 1 = return 1
fibr 2 = return 1
fibr n = do
  p1 <- ask
  p2 <- local (const (p1+1)) $ fibr (n-2)          
  f1 <- fibr (n-1)
  f2 <- fibr (n-2)
  return (p1+f1+f2+p2)

runFibr r = (`runReader`r) . fibr

fibmr 0 = return 0
fibmr 1 = return 1
fibmr 2 = return 1
fibmr n = do
  p1 <- ask
  p2 <- local (const (p1+1)) $ fibmr `memo` (n-2)          
  f1 <- fibmr `memo` (n-1)
  f2 <- fibmr `memo` (n-2)
  return (p1+f1+f2+p2)

runFibmr r = startEvalMemo . (`runReaderT`r) . fibmr

prop_ReaderEqv :: SmallInt Int -> SmallInt Int -> Bool
prop_ReaderEqv r n =
    ((`runReader`(toInt r)) . fibr  $ (toInt n)) == (startEvalMemo . (`runReaderT`(toInt r)) . fibmr $ (toInt n))


-- | With WriterT
fibw 0 = return 0
fibw 1 = return 1
fibw n = do
  f1 <- fibw (n-1)
  f2 <- fibw (n-2)
  tell $ show n
  return (f1+f2)

fibmw 0 = return 0
fibmw 1 = return 1
fibmw n = do
  f1 <- fibmw `memo` (n-1)
  f2 <- fibmw `memo` (n-2)
  tell $ show n
  return (f1+f2)

prop_WriterEqv :: SmallInt Int  -> Bool
prop_WriterEqv n =
    (runWriter . fibw . toInt $ n) == (startEvalMemo . runWriterT . fibmw . toInt $ n)


-- | With ContT
fibc 0 = return 0
fibc 1 = return 1
fibc n = do
  f1 <- fibc (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else fibc (n-2)
  return (f1+f2)

fibmc 0 = return 0
fibmc 1 = return 1
fibmc n = do
  f1 <- fibmc `memo` (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else fibmc `memo` (n-2)
  return (f1+f2)

prop_ContEqv :: SmallInt Int -> Bool
prop_ContEqv n =
    ((`runCont`id) . fibc . toInt $ n) == (startEvalMemo . (`runContT`return) . fibmc . toInt $ n)



-- | With StateT
fibs 0 = return 0
fibs 1 = return 1
fibs n = do
  s <- get
  f1 <- fibs (n-1)
  f2 <- fibs (n-2)
  modify $ \s -> s+1
  return (f1+f2+s)

fibms 0 = return 0
fibms 1 = return 1
fibms n = do
  s <- get
  f1 <- fibms `memo` (n-1)
  f2 <- fibms `memo` (n-2)
  modify $ \s -> s+1
  return (f1+f2+s)

prop_StateEqv :: SmallInt Int -> SmallInt Int -> Bool
prop_StateEqv s n =
    ((`runState`(toInt s)) . fibs . toInt $ n) == (startEvalMemo . (`runStateT`(toInt s)) . fibms . toInt $ n)




-- | With ListT
--
data Tree a = Leaf !a | Fork (Tree a) (Tree a) deriving Eq

partitions as = [ splitAt n as | n <- [1..length as - 1 ]]

unfringe [a] = [Leaf a]
unfringe as  = do
  (l,k) <- partitions as
  t <- unfringe l
  u <- unfringe k
  return (Fork t u)

unfringem [a] = return (Leaf a)
unfringem as = do
  (l,k) <- ListT $ return (partitions as)
  t <- unfringem `memo` l
  u <- unfringem `memo` k
  return (Fork t u)

prop_ListEqv :: SmallList Char -> Bool
prop_ListEqv ls =
    unfringe (toList ls) == (startEvalMemo . runListT . unfringem $ (toList ls))


-- | Mutual recursion
f :: Int -> (Int,String)
f 0 = (1,"+")
f n =(g((n-1),fst(f (n-1))),"-" ++ snd(f (n-1)))
g :: (Int, Int) -> Int
g (0, m)  = m + 1
g (n,m) = fst(f (n-1))-g((n-1),m)

type MemoF = MemoT Int (Int,String)
type MemoG = Memo (Int,Int) Int
type MemoFG = MemoF MemoG

fm :: Int -> MemoFG (Int,String)
fm 0 = return (1,"+")
fm n = do
  fn <- fm `memol0` (n-1)
  g <- gm `memol1` (n-1 , fst fn)
  return (g , "-" ++ snd fn)

gm :: (Int,Int) -> MemoFG Int
gm (0,m) = return (m+1) 
gm (n,m) = do
  fn <- fm `memol0` (n-1)
  g <- gm `memol1` (n-1,m)
  return $ fst fn - g

evalAll = startEvalMemo . startEvalMemoT
evalFm = evalAll . fm
evalGm = evalAll . gm


prop_MutualFEqv :: SmallInt Int -> Bool
prop_MutualFEqv sx  = f x == evalFm x
      where x = toInt sx

prop_MutualGEqv :: SmallInt Int -> SmallInt Int -> Bool
prop_MutualGEqv sx sy = g (x,y) == evalGm (x,y)
      where
        x = toInt sx
        y = toInt sy




tests = [
        testGroup "Transformers" [
                       testProperty "ReaderEqv"  prop_ReaderEqv,
                       testProperty "WriterEqv"  prop_WriterEqv,
                       testProperty "ContEqv"    prop_ContEqv,
                       testProperty "ListEqv"    prop_ListEqv,
                       testProperty "StateEqv"   prop_StateEqv
                      ],
        testGroup "Others" [
                       testProperty "MutualFGEqv"  prop_MutualFEqv
                       ]
    ]
