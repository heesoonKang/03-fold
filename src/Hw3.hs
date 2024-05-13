{- | CSE 130: All about fold.

     For this assignment, you may use the following library functions:

     length
     append (++)
     map
     foldl'
     foldr
     unzip
     zip
     reverse

  Use www.haskell.org/hoogle to learn more about the above.

  Do not change the skeleton code! The point of this assignment
  is to figure out how the functions can be written this way
  (using fold). You may only replace the `error "TBD:..."` terms.

-}

module Hw3 where

import Prelude hiding (replicate, sum)
import Data.List (foldl')

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft = foldl'

--------------------------------------------------------------------------------
-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30

-- cited: https://www.haskell.org/hoogle
sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
   f acc x = acc + x * x
   base  = 0

--------------------------------------------------------------------------------
-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24
-- cited: https://www.haskell.org/hoogle
pipe :: [(a -> a)] -> (a -> a)
pipe fs   = foldLeft f base fs
  where
    f acc x = acc . x
    base  = \x -> x

--------------------------------------------------------------------------------
-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"
-- cited: https://www.haskell.org/hoogle
sepConcat :: String -> [String] -> String
sepConcat sep []     = ""
sepConcat sep (x:xs) = foldLeft f base l
  where
    f acc x            = acc ++ sep ++ x
    base             = x
    l                = xs

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------
-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"
-- cited: https://www.haskell.org/hoogle
stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ sepConcat ", " (map f xs) ++ "]"

--------------------------------------------------------------------------------
-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]
-- cited: https://www.haskell.org/hoogle
clone :: a -> Int -> [a]
clone x n
  | n == 0 = []
  | otherwise = x : clone x (n - 1)

type BigInt = [Int]

--------------------------------------------------------------------------------
-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- [0,0,9,9] [1,0,0,2]
-- >>> padZero [1,0,0,2] [9,9]
-- [1,0,0,2] [0,0,9,9]
-- cited: https://www.haskell.org/hoogle
padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2
  | length l1 < length l2 = (clone 0 (length l2 - length l1) ++ l1, l2)
  | length l2 < length l1 = (l1, clone 0 (length l1 - length l2) ++ l2)
  | otherwise = (l1, l2)

--------------------------------------------------------------------------------
-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []
-- cited: https://www.haskell.org/hoogle
removeZero :: BigInt -> BigInt
removeZero ds = dropWhile (== 0) ds


--------------------------------------------------------------------------------
-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]
-- cited: https://www.haskell.org/hoogle
bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2     = removeZero res
  where
    (l1', l2')   = padZero l1 l2
    res          = foldLeft f base args
    f (x:xs) (n1, n2) = (x + n1 + n2) `div` 10 : ((x + n1 + n2) `mod` 10 : xs)
    base         = [0]
    args         = reverse (zip l1' l2')

--     bigAdd :: BigInt -> BigInt -> BigInt
-- bigAdd l1 l2     = removeZero res
--   where
--     (l1', l2')   = padZero l1 l2
--     res          = foldLeft f base args
--     f acc x        = error "TBD:bigAdd:f"
--     base         = error "TBD:bigAdd:base"
--     args         = error "TBD:bigAdd:args"


--------------------------------------------------------------------------------
-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]
-- cited: https://www.haskell.org/hoogle
mulByDigit :: Int -> BigInt -> BigInt
mulByDigit i n
           | i == 0    = []
           | i == 1    = (n)
           | otherwise = (bigAdd (mulByDigit (i - 1) n) n)

--------------------------------------------------------------------------------
-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]
-- cited: https://www.haskell.org/hoogle
bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldLeft f base args
    f (a1, a2) x    = (a1 + 1, bigAdd a2 (mulByDigit x l2 ++ clone 0 a1))
    base     = (0, [0])
    args     = reverse l1