{- |
Module      :  Sample.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Samples of usage of MemoT

-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Example.Memo
    (
         -- * Memoized Fibonacci number function
         fibm,
         evalFibm,

         -- * Combining ListT and MemoT transformers 
         -- | Original sample is taken from: \"Monadic Memoization Mixins\" by Daniel Brown and William R. Cook <http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf>

         -- ***    Non-memoized original definition
         Tree(..),
         fringe,
         unfringe,

         -- ***    Memoized definition
         unfringem,
         evalUnfringem,

         -- * Mutualy recursive function definitions
         -- | Original sample is taken from: \"Monadic Memoization Mixins\" by Daniel Brown and William R. Cook <http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf>

         -- ***    Non-memoized original definition
         f, g,

         -- ***    Memoized definition
         MemoF,
         MemoG,
         MemoFG,
         fm, gm,
         evalFm,
         evalGm,
                
         -- * Fibonacci with mutual recursive addition
         MemoFib,
         MemoBoo,
         MemoFB,
         boo,
         fibm2,
         evalFibM2,

         -- * Fibonacci with Memo and Writer
         fibmw,
         evalFibmw,

         -- * Fibonacci with MonadMemo and MonadCont
         fibmc,
         evalFibmc,

         -- * Tribonacci with constant factor through Reader plus memoization via Memo
         fibmr,
         evalFibmr,

         -- * Ackerman function
         ack,
         ackm,
         evalAckm,

) where

import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.Strict
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Writer

import Debug.Trace



fibm :: (Ord n, Num n) => n -> Memo n n n
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  n1 <- fibm `memo` (n-1)
  n2 <- fibm `memo` (n-2)
  return (n1+n2)

evalFibm :: Integer -> Integer
evalFibm = startEvalMemo . fibm


--
data Tree a = Leaf !a | Fork !(Tree a) !(Tree a) deriving (Show,Eq)

fringe :: Tree a -> [a]
fringe (Leaf a) = [a]
fringe (Fork t u) = fringe t ++ fringe u

partitions as = [ splitAt n as | n <- [1..length as - 1 ]]

-- | Non-memoized version (Uses ListT monad - returns a list of 'Tree')
unfringe ::  (Show t) => [t] -> [Tree t]
unfringe [a] =  show [a] `trace` [Leaf a]
unfringe as  =  show as `trace` do
  (l,k) <- partitions as
  t <- unfringe l
  u <- unfringe k
  return (Fork t u)


-- | Mixes memoization with ListT monad:
-- memoizes the result as list of 'Tree' (e.g. @k :: [t]@, @v :: [Tree t]@)
unfringem :: (Ord t, Show t) => [t] -> ListT (Memo [t] [Tree t]) (Tree t)
unfringem [a] = show [a] `trace` return (Leaf a)
unfringem as = show as `trace` do
  (l,k) <- ListT $ return (partitions as)
  t <- unfringem `memo` l
  u <- unfringem `memo` k
  return (Fork t u)

evalUnfringem :: (Ord t, Show t) => [t] -> [Tree t]
evalUnfringem = startEvalMemo . runListT . unfringem


-- | 'f' depends on 'g'
f :: Int -> (Int,String)
f 0 = (1,"+")
f (n+1)	=(g(n,fst(f n)),"-" ++ snd(f n))

-- | 'g' depends on 'f'
g :: (Int, Int) -> Int
g (0, m)  = m + 1
g (n+1,m) = fst(f n)-g(n,m)

-- | Memo-cache for 'fm'
type MemoF = MemoT Int (Int,String)
-- | Memo-cache for 'gm'
type MemoG = MemoT (Int,Int) Int

-- | Combined stack of caches (transformers)
-- Stacks two 'MemoT' transformers in one monad to be used in both 'gm' and 'fm' monadic functions
type MemoFG = MemoF (MemoG Identity)

fm :: Int -> MemoFG (Int,String)
fm 0 = return (1,"+")
fm (n+1) = do
  fn <- fm `memol0` n
  gn <- gm `memol1` (n , fst fn)
  return (gn , "-" ++ snd fn)

gm :: (Int,Int) -> MemoFG Int
gm (0,m) = return (m+1) 
gm (n+1,m) = do
  fn <- fm `memol0` n
  gn <- gm `memol1` (n,m)
  return $ fst fn - gn

evalAll = startEvalMemo . startEvalMemoT

-- | Function to run 'fm' computation
evalFm :: Int -> (Int, String)
evalFm = evalAll . fm

-- | Function to run 'gm' computation
evalGm :: (Int,Int) -> Int
evalGm = evalAll . gm



--
type MemoFib = MemoT Integer Integer
type MemoBoo = MemoT Double String
type MemoFB = MemoFib (MemoBoo Identity)

boo :: Double -> MemoFB String
boo 0 = "boo: 0" `trace` return ""
boo n = ("boo: " ++ show n) `trace` do
  n1 <- boo `memol1` (n-1)
  fn <- fibm2 `memol0` floor (n-1)
  return (show fn ++ n1)

fibm2 :: Integer -> MemoFB Integer 
fibm2 0 = "fib: 0" `trace` return 0
fibm2 1 = "fib: 1" `trace` return 1
fibm2 n = ("fib: " ++ show n) `trace` do
  l <- boo `memol1` fromInteger n
  f1 <- fibm2 `memol0` (n-1)
  f2 <- fibm2 `memol0` (n-2)
  return (f1 + f2 + floor (read l))

evalFibM2 :: Integer -> Integer
evalFibM2 = startEvalMemo . startEvalMemoT . fibm2




-- | Here we use monomorphic type
fibmw :: Integer -> WriterT String (Memo Integer (Integer,String)) Integer
fibmw 0 = "fib: 0" `trace` return 0
fibmw 1 = "fib: 1" `trace` return 1
fibmw n = ("fib: " ++ show n) `trace` do
  f1 <- fibmw `memo` (n-1)
  f2 <- fibmw `memo` (n-2)
  tell $ show n
  return (f1+f2)

evalFibmw :: Integer -> (Integer, String)
evalFibmw = startEvalMemo . runWriterT . fibmw

runFibmw = startRunMemo . runWriterT . fibmw


-- | Can also be defined with polymorphic monad classes
fibmc :: (Num t, Num b, MonadCont m, MonadMemo t b m) => t -> m b
fibmc 0 = "fib: 0" `trace` return 0
fibmc 1 = "fib: 1" `trace` return 1
fibmc n = ("fib: " ++ show n) `trace` do
  f1 <- fibmc `memo` (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else fibmc `memo` (n-2)
  return (f1+f2)

evalFibmc :: Integer -> Integer
evalFibmc = startEvalMemo . (`runContT`return) . fibmc

runFibmc = startRunMemo . (`runContT`return) . fibmc


fibmr :: (Num t, Num a, MonadMemo t a m, MonadReader a m) => t -> m a
fibmr 0 = "fib: 0" `trace` return 0
fibmr 1 = "fib: 1" `trace` return 1
fibmr 2 = "fib: 2" `trace` return 1
fibmr n = ("fib: " ++ show n) `trace` do
  p1 <- ask
  p2 <- local (const p1) $ fibmr `memo` (n-2)          
  f1 <- fibmr `memo` (n-1)
  f2 <- fibmr `memo` (n-2)
  return (p1+f1+f2+p2)

evalFibmr :: Integer -> Integer -> Integer
evalFibmr r = startEvalMemo . (`runReaderT` r) . fibmr

runFibmr r = startRunMemo . (`runReaderT` r) . fibmr




-- Ackerman function
ack :: Integer -> Integer -> Integer
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

ackm :: (Integer,Integer) -> Memo (Integer,Integer) Integer Integer
ackm (0,n) = return (n+1)
ackm (m,0) = ackm `memo` ((m-1),1)
ackm (m,n) = do
  n1 <- ackm `memo` (m,(n-1))
  ackm `memo` ((m-1),n1)

evalAckm :: Integer -> Integer -> Integer
evalAckm n m = startEvalMemo $ ackm (n,m)

runAckm n m = startRunMemo $ ackm (n,m)