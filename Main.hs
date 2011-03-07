
module Main where

import Sample.Memo
--import Control.Monad.Memo.Class
--import Control.Monad.Trans.Memo.Cu
--import Control.Monad.List
--import Control.Monad.Identity



--main = print $ length $ unfringe [1..14]
main = print $ length $ runUnfringem1 [1..14]