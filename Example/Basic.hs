{- |
Module      :  Sample.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

Some basic examples of 'monad-memo' usage

-}

{-# LANGUAGE FlexibleContexts #-}

module Example.Basic
    (
         -- * Memoized Fibonacci number function
         fibm,
         evalFibm,
         runFibm,

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

         -- * Fibonacci with `Memo` and `Writer`
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

         -- * Levensthein distance 
         editDistance,
         editDistancem,

         -- * Travelling salesman problem
         evalTsp,
         evalTspSTU,

         -- * Different MonadCache for the same monadic function
         -- ** `Data.IntMap`-based
         evalFibmIM,
         -- ** `ArrayCache`-based
         evalFibmSTA,
         evalFibmIOA,
         runFibmIOA,
         evalFibmIOUA,
         runFibmIOUA,
         evalFibmSTUA,
         runFibmSTUA,
         -- ** `VectorCache`-based
         evalFibmSTV,
         evalFibmSTUV,
         evalFibmIOV,
         evalFibmIOUV

) where

import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Control.Applicative

import Debug.Trace
import Data.Array.MArray
import Data.Array.IO

import Control.Monad.Memo
import Control.Monad.Memo.Vector.Expandable as EV




-- infix form
fibm' :: (Num n, Ord n) => n -> Memo n n n
fibm' 0 = return 0
fibm' 1 = return 1
fibm' n = memo fibm' (n-1) `mp` memo fibm' (n-2)
    where mp = liftM2 (+)

-- applicative form
fibm'' :: (Num n, Ord n) => n -> Memo n n n
fibm'' 0 = return 0
fibm'' 1 = return 1
fibm'' n = (+) <$> memo fibm'' (n-1) <*> memo fibm'' (n-2)


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
  t <- memo unfringem l
  u <- memo unfringem k
  return (Fork t u)

evalUnfringem :: (Ord t, Show t) => [t] -> [Tree t]
evalUnfringem = startEvalMemo . runListT . unfringem


-- | 'f' depends on 'g'
f :: Int -> (Int,String)
f 0 = (1,"+")
f n = (g(n,fst(f (n-1))),"-" ++ snd(f (n-1)))

-- | 'g' depends on 'f'
g :: (Int, Int) -> Int
g (0, m)  = m + 1
g (n,m) = fst(f (n-1))-g((n-1),m)

-- | Memo-cache for 'fm'
type MemoF = MemoT Int (Int,String)
-- | Memo-cache for 'gm'
type MemoG = MemoT (Int,Int) Int

-- | Combined stack of caches (transformers)
-- Stacks two 'MemoT' transformers in one monad to be used in both 'gm' and 'fm' monadic functions
type MemoFG = MemoF (MemoG Identity)

fm :: Int -> MemoFG (Int,String)
fm 0 = return (1,"+")
fm n = do
  fn <- memol0 fm (n-1)
  gn <- memol1 gm ((n-1) , fst fn)
  return (gn , "-" ++ snd fn)

gm :: (Int,Int) -> MemoFG Int
gm (0,m) = return (m+1) 
gm (n,m) = do
  fn <- memol0 fm (n-1)
  gn <- memol1 gm ((n-1),m)
  return $ fst fn - gn

evalAll = startEvalMemo . startEvalMemoT

-- | Function to run 'fm' computation
evalFm :: Int -> (Int, String)
evalFm = evalAll . fm

-- | Function to run 'gm' computation
evalGm :: (Int,Int) -> Int
evalGm = evalAll . gm


fm2 :: Int -> MemoFG (Int,String)
fm2 0 = return (1,"+")
fm2 n = do
  fn <- memol0 fm2 (n-1)
  gn <- for2 memol1 gm2 (n-1) (fst fn)
  return (gn , "-" ++ snd fn)

-- | Same as @gm@ but in curried form
gm2 :: Int -> Int -> MemoFG Int
gm2 0 m = return (m+1) 
gm2 n m = do
  fn <- memol0 fm2 (n-1)
  gn <- for2 memol1 gm2 (n-1) m
  return $ fst fn - gn


evalFm2 :: Int -> (Int, String)
evalFm2 = evalAll . fm2

evalGm2 :: Int -> Int -> Int
evalGm2 n m = evalAll $ gm2 n m





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




-- | Plus MonadWriter
fibmw 0 = "fib: 0" `trace` tell "0" >> return 0
fibmw 1 = "fib: 1" `trace` tell "1" >> return 1
fibmw n = ("fib: " ++ show n) `trace` do
  f1 <-  fibmw (n-1)
  f2 <-  fibmw (n-2)
  tell $ show n
  return (f1+f2)


evalFibmw :: Integer -> (Integer, String)
evalFibmw = startEvalMemo . runWriterT . fibmw

t1 n = startEvalMemo . runWriterT $ fibmw n >> fibmw 1 
t2 n = runWriter $ fibmw n >> fibmw 1 

runFibmw n = startRunMemo . runWriterT $ fibmw n >> fibmw 1

evalFibmwSTA n = runST $ evalArrayMemo (runWriterT (fibmw n)) (0,n)

evalFibmwSTV n = runST $ evalVectorMemo (runWriterT (fibmw n)) n

runFibmwST :: Integer -> ((Integer,String), Array Integer (Maybe (Integer,String)))
runFibmwST n = runST $ do
   (a,arr) <- runArrayMemo (runWriterT (fibmw n)) (0,n)
   iarr <- freeze arr
   return (a,iarr)

evalFibmwIO :: Integer -> IO (Integer, String)
evalFibmwIO n = evalArrayMemo (runWriterT (fibmw n)) (0,n)


-- | Can also be defined with polymorphic monad classes
-- MonadCont here
fibmc :: (Eq k, Num k, Show k, Num n, MonadCont m, MonadMemo k n m) => k -> m n
fibmc 0 = "fib: 0" `trace` return 0
fibmc 1 = "fib: 1" `trace` return 1
fibmc n = ("fib: " ++ show n) `trace` do
  f1 <- memo fibmc (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else memo fibmc (n-2)
  return (f1+f2)

evalFibmc :: Integer -> Integer
evalFibmc = startEvalMemo . (`runContT`return) . fibmc

runFibmc = startRunMemo . (`runContT`return) . fibmc

evalFibmcIO :: Integer -> IO Integer
evalFibmcIO n = (`evalArrayMemo`(0,n)) . (`runContT`return) . fibmc $ n

evalFibmcST :: Integer -> Integer
evalFibmcST n = runST $ (`evalArrayMemo`(0,n)) $ (`runContT`return) $ fibmc n


fibmr :: (Eq k, Num k, Show k, Num n, MonadMemo k n m, MonadReader n m) => k -> m n
fibmr 0 = "fib: 0" `trace` return 0
fibmr 1 = "fib: 1" `trace` return 1
fibmr 2 = "fib: 2" `trace` return 1
fibmr n = ("fib: " ++ show n) `trace` do
  p1 <- ask
  p2 <- local (const p1) $ memo fibmr (n-2)          
  f1 <- memo fibmr (n-1)
  f2 <- memo fibmr (n-2)
  return (p1+f1+f2+p2)

evalFibmr :: Integer -> Integer -> Integer
evalFibmr r = startEvalMemo . (`runReaderT` r) . fibmr

runFibmr r = startRunMemo . (`runReaderT` r) . fibmr



fibi 0 = print 0 >> return 0
fibi 1 = print 1 >> return 1
fibi n = do
  n1 <- fibi (n-1)
  n2 <- fibi (n-2)
  let r = n1+n2
  print r >> return r


fibmi 0 = print 0 >> return 0
fibmi 1 = print 1 >> return 1
fibmi n = do
  n1 <- memo fibmi (n-1)
  n2 <- memo fibmi (n-2)
  let r = n1+n2
  print r >> return r





-- | Ackerman function
ack :: (Eq n, Num n) => n -> n -> n
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

ackm :: (Num n, Ord n, MonadMemo (n, n) n m) => n -> n -> m n
ackm 0 n = return (n+1)
ackm m 0 = for2 memo ackm (m-1) 1
ackm m n = do
  n1 <- for2 memo ackm m (n-1)
  for2 memo ackm (m-1) n1

evalAckm :: (Num n, Ord n) => n -> n -> n
evalAckm n m = startEvalMemo $ ackm n m

runAckm n m = startRunMemo $ ackm n m

evalAckmST :: Int -> Int -> Int
evalAckmST n m = runST $ evalUArrayMemo (ackm n m) ((0,0),(4,100000))


-- | Levensthein distance - recursive definition
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance (x:xs) (y:ys) 
  | x == y = editDistance xs ys
  | otherwise = minimum [
      1 + editDistance xs (y:ys),
      1 + editDistance (x:xs) ys,
      1 + editDistance xs ys]

-- | Levensthein distance - with memoization
editDistancem [] ys = return $ length ys
editDistancem xs [] = return $ length xs
editDistancem (x:xs) (y:ys) 
  | x == y = for2 memo editDistancem xs ys
  | otherwise = ((+1) . minimum) <$> sequence [
      for2 memo editDistancem xs (y:ys),
      for2 memo editDistancem (x:xs) ys,
      for2 memo editDistancem xs ys]

runEditDistancem xs ys = startEvalMemo $ editDistancem xs ys


-- | Travelling salesman problem
tsp gph mp t ss
    | ss == (mp ! t) = return (gph ! (1,t))
    | otherwise = do
  krs <- mapM (\k -> for2 memo (tsp gph mp) k ss' >>= \r -> return (k,r)) (elms ss')
  return $ minimum [ r + gph ! (k,t) | (k,r) <- krs]
   where
     ss' = ss - (mp ! t)

elms ss = go 1 ss
    where
      go b 1 = [b]
      go b ss =
          case ss `quotRem` 2 of
            (q,1) -> b : go (b+1) q
            (q,0) -> go (b+1) q

calcTsp dim =  do
  rs <- mapM (\k -> for2 memo (tsp gph mp) k (ss-1)) [2..n]
  return $ minimum [ r + gph ! (k,1) | (r,k) <- zip rs [2..n]]
    where
      n = dim^2
      cities = [(x*dim+y+1, (fromIntegral x, fromIntegral y))
                    | x <- [0..dim-1], y <- [0..dim-1]]
      dists  = [((c1,c2), sqrt ((x1-x2)^2 + (y1-y2)^2))
                    | (c1,(x1,y1)) <- cities, (c2,(x2,y2)) <- cities]
      gph = array ((1,1),(n,n)) dists :: UArray (Int,Int) Float
      mp = array (1,n) [(i,2^(i-1)) | i <- [1..n]] :: UArray Int Int
      ss = 2^n-1

evalTsp = startEvalMemo . calcTsp

evalTspSTU dim = runST $ evalUArrayMemo (calcTsp dim) ((1,1),(n,2^n-1))
    where n = dim^2

evalTspIOU :: Int -> IO Float
evalTspIOU dim = evalUArrayMemo (calcTsp dim) ((1,1),(n,2^n-1))
    where n = dim^2


-- | Different `MonadCache` implementations
--   The same monadic funtion can be called using different MonadeCache implementation
 
fibm :: (Eq k, Num k, Num n, MonadMemo k n m) => k -> m n
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  n1 <- memo fibm (n-1)
  n2 <- memo fibm (n-2)
  return (n1+n2)


evalFibm :: Integer -> Integer
evalFibm = startEvalMemo . fibm

runFibm :: Integer -> (Integer, M.Map Integer Integer)
runFibm = startRunMemo . fibm

evalFibmIM :: Int -> Int
evalFibmIM n = evalMemoState (fibm n) IM.empty

evalFibmSTA :: Integer -> Integer
evalFibmSTA n = runST $ evalArrayMemo (fibm n) (0,n)

runFibmSTA :: Integer -> (Integer, Array Integer (Maybe Integer))
runFibmSTA n = runST $ do
  (a,arr) <- runArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (a, iarr)


evalFibmIOA :: Integer -> IO Integer
evalFibmIOA n = evalArrayMemo (fibm n) (0,n)

runFibmIOA :: Integer -> IO (Integer, Array Integer (Maybe Integer))
runFibmIOA n = do
  (r, arr) <- runArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (r, iarr)

evalFibmIOUA :: Int -> IO Int
evalFibmIOUA n = evalUArrayMemo (fibm n) (0,n) 

runFibmIOUA :: Int -> IO (Int, UArray Int Int)
runFibmIOUA n = do
  (r, arr) <- runUArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (r, iarr)

evalFibmSTUA :: Int -> Int
evalFibmSTUA n = runST $ evalUArrayMemo (fibm n) (0,n)

runFibmSTUA :: Int -> (Int, UArray Int Int)
runFibmSTUA n = runST $ do
    (a,arr) <- runUArrayMemo (fibm n) (0,n)
    iarr <- freeze arr
    return (a,iarr)


evalFibmSTV :: Int -> Integer
evalFibmSTV n = runST $ evalVectorMemo (fibm n) (n+1)

evalFibmIOV :: Int -> IO Integer
evalFibmIOV n = evalVectorMemo (fibm n) (n+1)

evalFibmSTUV :: Int -> Int
evalFibmSTUV n = runST $ evalUVectorMemo (fibm n) (n+1)

runFibmSTUV :: Int -> (Int, UV.Vector Int)
runFibmSTUV n = runST $ do
    (a,vec) <- runUVectorMemo (fibm n) (n+1)
    ivec <- UV.freeze vec
    return (a,ivec)

evalFibmIOUV :: Int -> IO Int
evalFibmIOUV n = evalUVectorMemo (fibm n) (n+1)

runFibmIOUV :: Int -> IO (Int, UV.Vector Int)
runFibmIOUV n = do
  (a, vec) <- runUVectorMemo (fibm n) (n+1)
  ivec <- UV.freeze vec
  return (a, ivec)


evalFibmSTEV :: Int -> Integer
evalFibmSTEV n = runST $ EV.startEvalVectorMemo (fibm n)

evalFibmIOEV :: Int -> IO Integer
evalFibmIOEV n = EV.startEvalVectorMemo (fibm n)

evalFibmSTEUV :: Int -> Int
evalFibmSTEUV n = runST $ EV.startEvalUVectorMemo (fibm n)

runFibmSTEUV :: Int -> (Int, UV.Vector Int)
runFibmSTEUV n = runST $ do
    (a,vec) <- EV.startRunUVectorMemo (fibm n)
    ivec <- UV.freeze vec
    return (a,ivec)

evalFibmIOEUV :: Int -> IO Int
evalFibmIOEUV n = EV.startEvalUVectorMemo (fibm n)

runFibmIOEUV :: Int -> IO (Int, UV.Vector Int)
runFibmIOEUV n = do
  (a, vec) <- EV.startRunUVectorMemo (fibm n)
  ivec <- UV.freeze vec
  return (a, ivec)