## Purpose
This package is aimed to provide a convenient mechanism to add memoization to Haskell monadic functions.

## Details
Memoization is a well known way to speed up calculation by caching calculated results and reusing them whenever a memoized function is called subsequently with the same arguments. Memoization is usually associated with Dynamic programming. Haskell, being pure (and lazy) functional language is especially well suited for memoization. Surprisingly, although one can easily find papers and packaged describing and implementing various memoization techniques, I couldn't find any commonly accepted standard. Hence here we have another implementation :)

## Basic ideas
The basic ideas behind this implementation the following:

* We will define memoization for monadic functions only
* We will use standard approach:
 1. Defining 'abstract interface' (typeclass) MonadMemo
 2. And then providing several implementation of this interface compatible with other monads via monad transformers mechanism
* Currently the default straightforward implementation MemoT internally uses Data.Map but additional, more sophisticated implementations can be added later


## Example of usage

Recursive Fibonacci number function in monadic form with memoization:

    fibm :: (Eq n, Num n, MonadMemo n n m) => n -> m n
    fibm 0 = return 0
    fibm 1 = return 1
    fibm n = do
      n1 <- memo fibm (n-1)
      n2 <- memo fibm (n-2)
      return (n1+n2)

    evalFibm :: Integer -> Integer
    evalFibm = startEvalMemo . fibm


## More Examples

### Simple recursive definition

Classic example of recursive function begging to be memoized is Fibonacci numbers function:

    fibm :: (Num n, MonadMemo n n m) => n -> m n
    fibm 0 = return 0
    fibm 1 = return 1
    fibm n = do
      n1 <- memo fibm (n-1)   -- Just add 'memo' before monadic function
      n2 <- memo fibm (n-2)
      return (n1+n2)

    evalFibm :: Integer -> Integer
    evalFibm = startEvalMemo . fibm

### Slightly more complicated recursive function
Well known [Ackerman function](http://en.wikipedia.org/wiki/Ackermann_function) is a two arguments function, here is a way to define it with memoization:

    ackm :: (Num n, Ord n, MonadMemo (n, n) n m) => n -> n -> m n
    ackm 0 n = return (n+1)
    ackm m 0 = for2 memo ackm (m-1) 1
    ackm m n = do
      n1 <- for2 memo ackm m (n-1)    -- 'for2' adapts 'memo' for 2-argument 'ackm' 
      for2 memo ackm (m-1) n1

    evalAckm :: (Num n, Ord n) => n -> n -> n
    evalAckm n m = startEvalMemo $ ackm n m

### Mutually recursive function memoization
This example is taken from paper ["Monadic Memoization Mixins" by Daniel Brown and William R. Cook](http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf)

Given the following mutually recursive function definitions:

    -- 'f' depends on 'g'
    f :: Int -> (Int,String)
    f 0 = (1,"+")
    f (n+1)	=(g(n,fst(f n)),"-" ++ snd(f n))

    -- 'g' depends on 'f'
    g :: (Int, Int) -> Int
    g (0, m)  = m + 1
    g (n+1,m) = fst(f n)-g(n,m)

How can we memoize both functions?

Lets try to just add [memo](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo.html#v:memo) for both functions:

    -- WRONG: Will NOT compile!
    fm 0 = return (1,"+")
    fm (n+1) = do
      fn <- memo fm n
      gn <- memo gm (n , fst fn)
      return (gn , "-" ++ snd fn)

    gm (0,m) = return (m+1) 
    gm (n+1,m) = do
      fn <- memo fm n
      gn <- memo gm (n,m)
      return $ fst fn - gn

GHC complains:

    "Occurs check: cannot construct the infinite type: t = (t, v)
         Expected type: t
   
         Inferred type: (t, v)"

which is understandable since we are trying to use the same cache for storing "key-value" pairs of the functions of different types (`fm :: Int -> m (Int,String)` and `gm :: (Int, Int) -> m Int`).
Obviously, to cache both function we will need _two_ caches (even if the types of the functions were identical, it's not very good idea to share the same cache).
And this is precisely what we have to do - use two memoization caches! The way to achieve it is to use _two_ [MemoT](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo.html#t:MemoT) monad transformers one nested in another:

    -- Memo-cache for 'fm'
    type MemoF = MemoT Int (Int,String)
    -- Memo-cache for 'gm'
    type MemoG = MemoT (Int,Int) Int

    -- | Combined stack of caches (transformers)
    -- Stacks two 'MemoT' transformers in one monad to be used in both 'gm' and 'fm' monadic functions
    type MemoFG = MemoF (MemoG Identity)

NB As usually with Haskell it isn't necessary to specify types here (or restrict them to [MemoT](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo.html#t:MemoT) combinations for the given example).

Then, a little bit of complication, since we use _two_ caches now (one from the current [monad transformer](http://en.wikibooks.org/wiki/Haskell/Monad_transformers) and another from the next, nested in the current) we need to use *memol_X_* set of functions: [memol0](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo.html#v:memol0), [memol1](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo.html#v:memol1) etc. Where _X_ specifies "sequential number" of the transformer in stack for a given cache (starting from the current). Here we use the current (0) and the next (1) for `fm` and `gm` respectively:

    fm :: Int -> MemoFG (Int,String)
    fm 0 = return (1,"+")
    fm (n+1) = do
      fn <- memol0 fm n
      gn <- memol1 gm (n , fst fn)
      return (gn , "-" ++ snd fn)

    gm :: (Int,Int) -> MemoFG Int
    gm (0,m) = return (m+1) 
    gm (n+1,m) = do
      fn <- memol0 fm n
      gn <- memol1 gm (n,m)
      return $ fst fn - gn

    evalAll = startEvalMemo . startEvalMemoT

    -- | Function to run 'fm' computation
    evalFm :: Int -> (Int, String)
    evalFm = evalAll . fm

    -- | Function to run 'gm' computation
    evalGm :: (Int,Int) -> Int
    evalGm = evalAll . gm

In fact we can also define 'gm' function in curried form:

    fm2 :: Int -> MemoFG (Int,String)
    fm2 0 = return (1,"+")
    fm2 n = do
      fn <- memol0 fm2 (n-1)
      gn <- for2 memol1 gm2 (n-1) (fst fn)
      return (gn , "-" ++ snd fn)

    -- 2-argument function now
    gm2 :: Int -> Int -> MemoFG Int
    gm2 0 m = return (m+1) 
    gm2 n m = do
      fn <- memol0 fm2 (n-1)
      gn <- for2 memol1 gm2 (n-1) m   -- 'for2' adapts 'memol1' for 2-argument gm2
      return $ fst fn - gn

    evalFm2 :: Int -> (Int, String)
    evalFm2 = evalAll . fm2
    
    evalGm2 :: Int -> Int -> Int
    evalGm2 n m = evalAll $ gm2 n m

### Combining MemoT with other monads
Being monad transformer, memoization monad can be combined with most of existing monads.
Here we mix it with [MonadWriter](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Writer-Class.html#t:MonadWriter):

    fibmw :: (Num n, MonadWriter String m, MonadMemo n n m) => n -> m n
    fibmw 0 = tell "0" >> return 0
    fibmw 1 = tell "1" >> return 1
    fibmw n = do
      f1 <- memo fibmw (n-1)
      f2 <- memo fibmw (n-2)
      tell $ show n
      return (f1+f2)

    -- To run combined monad we need to sequence both 'run' functions:
    evalFibmw :: Integer -> (Integer, String)
    evalFibmw = startEvalMemo . runWriterT . fibmw

    res = evalFibmw 6  -- > produces (8,"1021310241021351021310246")

## Custom cache container
From monad-memo [version 0.3.0](http://hackage.haskell.org/package/monad-memo-0.3.0] it is possible to replace default [Data.Map](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map.html) with another (more efficient?) implementation of internal cache-container as long as there is an instance of [Data.MapLike](http://hackage.haskell.org/packages/archive/monad-memo/0.3.0/doc/html/Data-MapLike.html) defined for that container.  The package currently defines these instances for [Data.Map](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-Map.html) and [Data.IntMap](http://hackage.haskell.org/packages/archive/containers/latest/doc/html/Data-IntMap.html) only (the only suitable container from [containers package](http://hackage.haskell.org/package/containers-0.4.0.0)).

Below is an example how we can switch to [unordered-containers](http://hackage.haskell.org/package/unordered-containers) (as [recommended](http://stackoverflow.com/questions/5515025/edit-distance-algorithm-in-haskell-performance-tuning) by [TomMD](http://stackoverflow.com/users/216164/tommd), thanks!):

The slightly modified original version:

    {-# LANGUAGE FlexibleContexts #-}
    
    import Control.Monad.Memo
    
    editDistance      :: (Eq a, Ord a) => [a] -> [a] -> Int
    editDistance s1 s2 = startEvalMemo $ editDistancem (1, 1, 1, s1, s2)

    -- weighted levenshtein distance
    -- ins, sub and del are the costs for the various operations
    editDistancem :: (MonadMemo (Int, Int, Int, [a], [a]) Int m, Eq a) => (Int, Int, Int, [a], [a]) -> m Int
    editDistancem (del, sub, ins, s1, s2)
      | null s2 = return $ ins * length s1
      | null s1 = return $ ins * length s2
      | last s1 == last s2 = memo editDistancem (del, sub, ins, (init s1), (init s2))
      | otherwise = do
            r1 <- memo editDistancem (del, sub, ins, s1, (init s2))
            r2 <- memo editDistancem (del, sub, ins, (init s1), (init s2))
            r3 <- memo editDistancem (del, sub, ins, (init s1), s2)
            return $ minimum [ r1 + del -- deletion 
                             , r2 + sub -- substitution
                             , r3 + ins -- insertion
                               ]
    
    str2 = replicate 100 'a' ++ replicate 100 'b'
    str1 = replicate 100 'b' ++ replicate 100 'c'
    
    main = print $ editDistance str1 str2

Which runs in around 8 seconds on my machine.

To switch to [unordered-containers](http://hackage.haskell.org/package/unordered-containers) we need to firstly install this package and then just to add the following instance definition:

    import Data.Hashable
    import qualified Data.HashMap.Lazy as L
    
    instance (Eq k, Hashable k) => MapLike (L.HashMap k v) k v where
        lookup = L.lookup
        add = L.insert

and change

    startEvalMemo

to

    (`evalMemoState`L.empty)

Here is the resulting code:

    {-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
    
    import Control.Monad.Memo
    import Data.Hashable
    import qualified Data.HashMap.Lazy as L
    
    instance (Eq k, Hashable k) => MapLike (L.HashMap k v) k v where
        lookup = L.lookup
        add = L.insert
    
    editDistance      :: (Eq a, Hashable a) => [a] -> [a] -> Int
    editDistance s1 s2 = (`evalMemoState`L.empty) $ editDistancem (1, 1, 1, s1, s2)
    
    -- No changes in the function itself
    editDistancem :: (MonadMemo (Int, Int, Int, [a], [a]) Int m, Eq a) => (Int, Int, Int, [a], [a]) -> m Int
    editDistancem (del, sub, ins, s1, s2)
      | null s2 = return $ ins * length s1
      | null s1 = return $ ins * length s2
      | last s1 == last s2 = memo editDistancem (del, sub, ins, (init s1), (init s2))
      | otherwise = do
            r1 <- memo editDistancem (del, sub, ins, s1, (init s2))
            r2 <- memo editDistancem (del, sub, ins, (init s1), (init s2))
            r3 <- memo editDistancem (del, sub, ins, (init s1), s2)
            return $ minimum [ r1 + del -- deletion 
                             , r2 + sub -- substitution
                             , r3 + ins -- insertion
                               ]
    
    str2 = replicate 100 'a' ++ replicate 100 'b'
    str1 = replicate 100 'b' ++ replicate 100 'c'
    
    main = print $ editDistance str1 str2

Which produces the same result in 1.5 seconds


## Mutable arrays as MonadCache
[version 0.4.0](http://hackage.haskell.org/package/monad-memo-0.4.0) adds [ArrayCache](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/monad-memo/Control-Monad-Trans-Memo-Array.html): a new [MonadCache](http://hackage.haskell.org/packages/archive/monad-memo/latest/doc/html/Control-Monad-Memo-Class.html#t:MonadCache) implementation based on mutable arrays (inside [IO](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html#t:IO) or [ST s](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad-ST.html) monad). The main benefit of this `MonadCache` is its performance: it can be an order of magnitude faser than standard `Data.Map`-based cache. This is due to the fact that arrays have `O(1)` lookup time and in-place mutable arrays also have `O(1)` for updates (i.e. the cache `add` operation).

Unfortunatelly you cannot always use this `MonadCache` due to array's natural limitations:

* The key must be an instance of [Ix](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Ix.html#t:Ix) typeclass   
* The bounds of the array must be known (and specified) beforehand and array cannot be resized
* Array is a continious space of values, so if the key distribution is wide and sparse the memory will be wasted (or array may not even fit into memory)

But if the nature of your memoized function permits the usage of `ArrayCache` you can make your code much more faster by simply switching from Map-based `MonadCache` to `ArrayCache` especially if the value type of your function can be "unboxed" (i.e. it is one of primitive types like `Int` or `Double`). "Unboxed" values are packed in unboxed arrays `UArray` which offer even faster execution and are the most efficient in terms of memory usage.
Normally you don't have to modify your monadic function definition to run `ArrayCache`-based memoization: just use appropriate `eval*` or `run*` function. For instance our canonical `fibm` function:

    fibm 0 = return 0
    fibm 1 = return 1
    fibm n = do
      n1 <- memo fibm (n-1)
      n2 <- memo fibm (n-2)
      return (n1+n2)

can be run using `ST` array of `Integers` with the following function:

    evalFibmST :: Integer -> Integer
    evalFibmST n = evalSTArrayMemo (fibm n) (0,n)

here the `(0,n)` argument defines the bounds of cache array.
Is it equally easy to use unboxed version of the array, but `Integer` cannot be unboxed (it isn't primitive type), so lets just use `Double` for our function result:

    evalFibmSTU :: Integer -> Double
    evalFibmSTU n = evalSTUArrayMemo (fibm n) (0,n)
 
Instead of `ST` you can use `IO` monad:

    evalFibmIO :: Integer -> IO Integer
    evalFibmIO n = evalIOArrayMemoM (fibm n) (0,n)
    
    evalFibmIOU :: Integer -> IO Double
    evalFibmIOU n = evalUArrayMemoM (fibm n) (0,n)

Note that there is only monadic `eval` for `IO` since the result cannot "escape" `IO` (but it is not the case for `ST`)

The difference in performance for different `MonadCache`'s (plus different memoization libraries) with Fibonacci function produced by the following [test](https://gist.github.com/4688576) (slightly modified version of [comatose/Memo.hs gist](https://gist.github.com/3968642)):

    pc$ ./Memo 100000
    monad-memo Map: 0.149449000 sec 
    monad-memo IntMap: 0.074370000 sec 
    monad-memo Hashtable: 0.185919000 sec 
    monad-memo ST Array: 0.049314000 sec 
    monad-memo ST UArray: 0.021682000 sec 
    memoize: 0.723779000 sec 
    State Map: 0.434514000 sec 
    State IntMap: 0.172484000 sec 
    MemoCombinators: 1.067222000 sec 
    MemoTrie: 0.418166000 sec 



## References
* http://www.haskell.org/haskellwiki/Memoization
* ["Monadic Memoization Mixins" by Daniel Brown and William R. Cook](http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf)
* [data-memocombinators](http://hackage.haskell.org/packages/archive/data-memocombinators/latest/doc/html/Data-MemoCombinators.html)
* ["Fun with Type Functions" by Oleg Kiselyov, Ken Shan, and Simon Peyton Jones (see 3.1 - "Type-directed memoization")](http://research.microsoft.com/~simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf)