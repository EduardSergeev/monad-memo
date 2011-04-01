{- |
Module      :  Control.Monad.Memo
Copyright   :  (c) Eduard Sergeev 2011
License     :  BSD-style (see the file LICENSE)

Maintainer  :  eduard.sergeev@gmail.com
Stability   :  experimental
Portability :  non-portable (multi-param classes, functional dependencies)

[Computation type:] Monadic computations with support for memoization.

Defines monadic interface 'MonadMemo' for memoization and simple implementation 'MemoT' (based on 'Data.Map')
-}


module Control.Monad.Memo (
    -- * MonadMemo class
    MonadMemo(..),
    -- * The Memo monad
    Memo,
    runMemo,
    evalMemo,
    startRunMemo,
    startEvalMemo,
    -- * The MemoT monad transformer
    MemoT(..),
    runMemoT,
    evalMemoT,
    startRunMemoT,
    startEvalMemoT,
    module Control.Monad,
    module Control.Monad.Trans,
    -- * Adapter for memoization of multi-argument functions
    for2,
    for3,
    for4,
    -- * Memoization cache level access functions         
    memoln,
    memol0,
    memol1,
    memol2,
    memol3,
    memol4,
    -- * Example 1: Fibonacci numbers
    -- $fibExample

    -- * Example 2: Mutualy recursive definition with memoization
    -- $mutualExample

    -- * Example 3: Combining Memo with other transformers
    -- $transExample

    -- * Example 4: Memoization of multi-argument function
    -- $multiExample
    ) where

import Control.Monad.Memo.Class

import Control.Monad.Trans.Memo.Strict (
    MemoT(..), runMemoT, startRunMemoT, evalMemoT, startEvalMemoT,
    Memo, runMemo, startRunMemo, evalMemo, startEvalMemo )

import Control.Monad
import Control.Monad.Trans

{- $fibExample
Memoization can be specified whenever monadic computation is taking place.
Including recursive definition. Classic example: Fibonacci number function:
Here is simple non-monadic definition of it

>fib :: (Num n) => n -> n
>fib 0 = 0
>fib 1 = 1
>fib n = fib (n-1) + fib (n-2)

To use 'Memo' monad we need to convert it into monadic form:

>fibm :: (Num n, Monad m) => n -> m n
>fibm 0 = return 0
>fibm 1 = return 1
>fibm n = do
>  n1 <- fibm (n-1)
>  n2 <- fibm (n-2)
>  return (n1+n2)

Then we can specify which computation we want to memoize with 'memo' (both recursive calls to (n-1) and (n-2)):

>fibm :: (Num n, Ord n) => n -> Memo n n n
>fibm 0 = return 0
>fibm 1 = return 1
>fibm n = do
>  n1 <- memo fibm (n-1)
>  n2 <- memo fibm (n-2)
>  return (n1+n2)

NB: 'Ord' is required since internaly Memo implementation uses 'Data.Map' to store and lookup memoized values

Then it can be run with 'startEvalMemo'

>startEvalMemo . fibm $ 5

Or using applicative form:

>fibm :: (Num n, Ord n) => n -> Memo n n n
>fibm 0 = return 0
>fibm 1 = return 1
>fibm n = (+) <$> memo fibm (n-1) <*> memo fibm (n-2)

-}

{- $mutualExample
In order to use memoization for both mutually recursive function we need to use nested MemoT monad transformers
(one for each cache). Let's extend our Fibonacci function with meaningless extra function @boo@ which in turn uses @fibm2@.

Memoization cache type for @fibm2@ (caches @Integer -> Integer@) will be:

>type MemoFib = MemoT Integer Integer

While cache for @boo@ (@Double -> String@):

>type MemoBoo = MemoT Double String

Stacking them together gives us te overall type for our combined memoization monad:

>type MemoFB = MemoFib (MemoBoo Identity)

>boo :: Double -> MemoFB String
>boo 0 = return ""
>boo n = do
>  n1 <- memol1 boo (n-1)           -- uses next in stack transformer (memol_1_): MemoBoo is nested in MemoFib
>  fn <- memol0 fibm2 floor (n-1)   -- uses current transformer (memol_0_): MemoFib
>  return (show fn ++ n1)

>fibm2 :: Integer -> MemoFB Integer 
>fibm2 0 = return 0
>fibm2 1 = return 1
>fibm2 n = do
>  l <- memol1 boo (fromInteger n)        -- as in 'boo' we need to use 1st nested transformer here
>  f1 <- memol0 fibm2 (n-1)               -- and 0st (the current) for fibm2
>  f2 <- memol0 fibm2 (n-2)
>  return (f1 + f2 + floor (read l))

>evalFibM2 = startEvalMemo . startEvalMemoT . fibm2

-}

{- $transExample
'MonadMemo' can be combined with other monads and transformers:

With 'MonadWriter':

>fibmw :: (Num n, MonadWriter String m, MonadMemo n n m) => n -> m n
>fibmw 0 = return 0
>fibmw 1 = return 1
>fibmw n = do
>  f1 <- memo fibmw (n-1)
>  f2 <- memo fibmw (n-2)
>  tell $ show n
>  return (f1+f2)

>evalFibmw = startEvalMemo . runWriterT . fibmw

-}

{- $multiExample
Functions with more than one argument (in curried form) can also be memoized with a help of @forX@ set of function:
For two-argument function we can use 'for2' function adapter:

>-- Ackerman function classic definition
>ack :: Num n => n -> n -> n
>ack 0 n = n+1
>ack m 0 = ack (m-1) 1
>ack m n = ack (m-1) (ack m (n-1))
>
>-- Ackerman function memoized definition
>ackm :: (Num n, Ord n, MonadMemo (n, n) n m) => n -> n -> m n
>ackm 0 n = return (n+1)
>ackm m 0 = for2 memo ackm (m-1) 1
>ackm m n = do
>  n1 <- for2 memo ackm m (n-1)
>  for2 memo ackm (m-1) n1
>
>evalAckm :: (Num n, Ord n) => n -> n -> n
>evalAckm n m = startEvalMemo $ ackm n m

-}
