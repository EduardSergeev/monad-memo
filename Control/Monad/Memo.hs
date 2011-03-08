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
    module Control.Monad.Fix,
    module Control.Monad.Trans,
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
    ) where

import Control.Monad.Memo.Class

import Control.Monad.Trans.Memo.Strict (
    MemoT(..), runMemoT, startRunMemoT, evalMemoT, startEvalMemoT,
    Memo, runMemo, startRunMemo, evalMemo, startEvalMemo )

import Control.Monad.Trans
import Control.Monad
import Control.Monad.Fix

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
>  n1 <- fibm `memo` (n-1)
>  n2 <- fibm `memo` (n-2)
>  return (n1+n2)

NB: 'Ord' is required since internaly Memo implementation uses 'Data.Map' to store and lookup memoized values

Then it can be run with 'startEvalMemo'

>startEvalMemo . fibm $ 5

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
>boo 0 = "boo: 0" `trace` return ""
>boo n = ("boo: " ++ show n) `trace` do
>  n1 <- boo `memol1` (n-1)           -- uses next in stack transformer (memol_1_): MemoBoo is nested in MemoFib
>  fn <- fibm2 `memol0` floor (n-1)   -- uses current transformer (memol_0_): MemoFib
>  return (show fn ++ n1)

>fibm2 :: Integer -> MemoFB Integer 
>fibm2 0 = "fib: 0" `trace` return 0
>fibm2 1 = "fib: 1" `trace` return 1
>fibm2 n = ("fib: " ++ show n) `trace` do
>  l <- boo `memol1` fromInteger n   -- as in 'boo' we need to use 1st nested transformer here
>  f1 <- fibm2 `memol0` (n-1)        -- and 0st (the current) for fibm2
>  f2 <- fibm2 `memol0` (n-2)
>  return (f1 + f2 + floor (read l))

>evalFibM2 = startEvalMemo . startEvalMemoT . fibm2

-}

{- $transExample
Being transformer, @MemoT@ can be used with other monads and transformers:

With @Writer@:

>fibmw 0 = return 0
>fibmw 1 = return 1
>fibmw n = do
>  f1 <- fibmw `memo` (n-1)
>  f2 <- fibmw `memo` (n-2)
>  tell $ show n
>  return (f1+f2)

>evalFibmw = startEvalMemo . runWriterT . fibmw

-}

