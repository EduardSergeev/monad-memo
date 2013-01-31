{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Memo.Test
(
       tests
) where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Random

import Control.Monad.Memo
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.List

import qualified Data.IntMap as IM

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


smallUpperBound :: Num n => n
smallUpperBound = 10

newtype SmallInt n = SmallInt { toInt::n } deriving Show

instance (Num n, Random n) => Arbitrary (SmallInt n) where
    arbitrary = fmap SmallInt $ choose (0,smallUpperBound)

newtype SmallList a = SmallList { toList::[a] } deriving Show

instance Arbitrary a => Arbitrary (SmallList a) where
    arbitrary = do
      n <- choose (0,10)
      ls <- arbitrary
      return $ SmallList $ take n ls 

medUpperBound :: Num n => n
medUpperBound = 1000

newtype MedInt n = MedInt { medToInt::n } deriving Show

instance (Num n, Random n) => Arbitrary (MedInt n) where
    arbitrary = fmap MedInt $ choose (0,medUpperBound)


-- | Plain monadic definition
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n-1)
  f2 <- memo fibm (n-2)
  return (f1+f2)


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
  p2 <- local (const (p1+1)) $ memo fibmr (n-2)          
  f1 <- memo fibmr (n-1)
  f2 <- memo fibmr (n-2)
  return (p1+f1+f2+p2)

runFibmr r = startEvalMemo . (`runReaderT`r) . fibmr

runFibmrST r n = runSTArrayMemo (runReaderT (fibmr n) r) ((0,0),(r+n,n))

prop_ReaderEqv :: SmallInt Int -> SmallInt Int -> Bool
prop_ReaderEqv r n =
    ((`runReader`(toInt r)) . fibr  $ (toInt n)) ==
    (startEvalMemo . (`runReaderT`(toInt r)) . fibmr $ (toInt n))

prop_ReaderSTEqv :: SmallInt Integer -> SmallInt Integer -> Bool
prop_ReaderSTEqv (SmallInt r) (SmallInt n) =
    runReader (fibr n) r ==
    evalSTArrayMemo (runReaderT (fibmr n) r) ((0,0),(r+n,n))


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
  f1 <- memo fibmw (n-1)
  f2 <- memo fibmw (n-2)
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
  f1 <- memo fibmc (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else memo fibmc (n-2)
  return (f1+f2)

prop_ContEqv :: SmallInt Int -> Bool
prop_ContEqv n =
    ((`runCont`id) . fibc . toInt $ n) ==
    (startEvalMemo . (`runContT`return) . fibmc . toInt $ n)


prop_ContSTUEqv :: SmallInt Int -> Bool
prop_ContSTUEqv (SmallInt n) =
    (runCont (fibc n) id :: Int) ==
    evalSTUArrayMemo (runContT (fibmc n) return) (0,n)


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
  f1 <- memo fibms (n-1)
  f2 <- memo fibms (n-2)
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
  t <- memo unfringem l
  u <- memo unfringem k
  return (Fork t u)

prop_ListEqv :: SmallList Char -> Bool
prop_ListEqv ls =
    unfringe (toList ls) == (startEvalMemo . runListT . unfringem $ (toList ls))


-- | Mutual recursion
f :: Int -> (Int,String)
f 0 = (1,"+")
f n = (g((n-1),fst(f (n-1))),"-" ++ snd(f (n-1)))
g :: (Int, Int) -> Int
g (0, m)  = m + 1
g (n,m) = fst(f (n-1))-g((n-1),m)

type MemoF = MemoT Int (Int,String)
type MemoG = Memo (Int,Int) Int
type MemoFG = MemoF MemoG

fm :: Int -> MemoFG (Int,String)
fm 0 = return (1,"+")
fm n = do
  fn <- memol0 fm (n-1)
  g <- memol1 gm (n-1 , fst fn)
  return (g , "-" ++ snd fn)

gm :: (Int,Int) -> MemoFG Int
gm (0,m) = return (m+1) 
gm (n,m) = do
  fn <- memol0 fm (n-1)
  g <- memol1 gm (n-1,m)
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

-- Same as above but without explicit uncurring
fm2 :: Int -> MemoFG (Int,String)
fm2 0 = return (1,"+")
fm2 n = do
  fn <- memol0 fm2 (n-1)
  g <- for2 memol1 gm2 (n-1) (fst fn)
  return (g , "-" ++ snd fn)

gm2 :: Int -> Int -> MemoFG Int
gm2 0 m = return (m+1) 
gm2 n m = do
  fn <- memol0 fm2 (n-1)
  g <- for2 memol1 gm2 (n-1) m
  return $ fst fn - g

evalAll2 = startEvalMemo . startEvalMemoT
evalFm2 = evalAll . fm2
evalGm2 n m = evalAll $ gm2 n m


prop_Mutual2FEqv :: SmallInt Int -> Bool
prop_Mutual2FEqv sx  = f x == evalFm2 x
      where x = toInt sx

prop_Mutual2GEqv :: SmallInt Int -> SmallInt Int -> Bool
prop_Mutual2GEqv sx sy = g (x,y) == evalGm2 x y
      where
        x = toInt sx
        y = toInt sy



-- | Different memo caches tests

fibMap :: (Ord n, Num n, Num v) => n -> v
fibMap = startEvalMemo . fibm 

fibIntMap :: Int -> Int
fibIntMap = (`evalMemoState`IM.empty) . fibm 

fibSTA :: Integer -> Integer
fibSTA n = evalSTArrayMemo (fibm n) (0,n)

fibSTUA :: Int -> Int
fibSTUA n = evalSTUArrayMemo (fibm n) (0,n)

fibSTUAD :: Int -> Double
fibSTUAD n = evalSTUArrayMemo (fibm n) (0,n)

fibIOA :: Integer -> IO Integer
fibIOA n = evalIOArrayMemoM (fibm n) (0,n)

fibIOUA :: Int -> IO Int
fibIOUA n = evalUArrayMemoM (fibm n) (0,n)


prop_IntMapEqv :: MedInt Int -> Bool
prop_IntMapEqv (MedInt n) = fibMap n == fibIntMap n

prop_STAEqv :: MedInt Integer -> Bool
prop_STAEqv (MedInt n) = fibMap n == fibSTA n

prop_STUAEqv :: MedInt Int -> Bool
prop_STUAEqv (MedInt n) = fibMap n == fibSTUA n

prop_STUADEqv :: MedInt Int -> Bool
prop_STUADEqv (MedInt n) = fibMap n == fibSTUA n


prop_IOAEqv :: MedInt Integer -> Property
prop_IOAEqv (MedInt n) = monadicIO $ do
                           r <- run $ fibIOA n
                           assert $ r == fibMap n

prop_IOUAEqv :: MedInt Int -> Property
prop_IOUAEqv (MedInt n) = monadicIO $ do
                           r <- run $ fibIOUA n
                           assert $ r == fibMap n

tests = [
        testGroup "Transformers" [
                       testProperty "ReaderEqv"         prop_ReaderEqv,
                       testProperty "ReaderSTEqv"       prop_ReaderSTEqv,
                       testProperty "WriterEqv"         prop_WriterEqv,
                       testProperty "ContEqv"           prop_ContEqv,
                       testProperty "ContSTUEqv"        prop_ContSTUEqv,
                       testProperty "ListEqv"           prop_ListEqv,
                       testProperty "StateEqv"          prop_StateEqv
                      ],
        testGroup "Others" [
                       testProperty "MutualFGEqv"       prop_MutualFEqv,
                       testProperty "MutualCurryFGEqv"  prop_Mutual2FEqv
                       ],
        testGroup "Different memo-caches" [
                       testProperty "Data.IntMap cache" prop_IntMapEqv,
                       testProperty "STArray cache"     prop_STAEqv,
                       testProperty "STUArray cache"    prop_STUAEqv,
                       testProperty "STUArray Double"   prop_STUADEqv,
                       testProperty "IOArray cache"     prop_IOAEqv,
                       testProperty "IOUArray cache"    prop_IOUAEqv
                      ]
    ]
