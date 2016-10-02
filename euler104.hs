--------------------------------------------------------------------------------
-- Project Euler
-- Problem 104
-- Pandigital Fibonacci ends
--------------------------------------------------------------------------------
-- The Fibonacci sequence is defined by the recurrence relation:
-- 
--     Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
-- 	
-- It turns out that F541, which contains 113 digits, is the first Fibonacci
-- number for which the last nine digits are 1-9 pandigital (contain all the
-- digits 1 to 9, but not necessarily in order). And F2749, which contains 575
-- digits, is the first Fibonacci number for which the first nine digits are
-- 1-9 pandigital.
-- 
-- Given that Fk is the first Fibonacci number for which the first nine digits
-- AND the last nine digits are 1-9 pandigital, find k.
--------------------------------------------------------------------------------

import Data.Word (Word64)
import Data.List (findIndices)

fibends = 0:1: zipWith addmod (fibends) (tail fibends) :: [Word64]
  where addmod a b = mod (a+b) (10^9)

pandigital :: String -> Bool
pandigital s = all (flip elem s) "123456789"

-- indices of fibs with pandigital ends
ends = findIndices (pandigital . show) fibends

-------------

fibstarts = (0,0):(1,0): zipWith add (fibstarts) (tail fibstarts) :: [(Word64,Int)]
  where truncate n | n > 10^18  = (div n 10, 1)
                   | otherwise = (n, 0)
        add (a,0) (b,1) = truncate (div a 10 + b)
        add (a,_) (b,_) = truncate (a+b)

-- inidices of fibs with pandigital start
starts = findIndices (pandigital . take 9 . show . fst) fibstarts

-----------

-- find all values which are an element of the two given lists
-- assuming both lists are strictly increasing
findeq :: Ord a => [a] -> [a] -> [a]
findeq [] _ = []
findeq _ [] = []
findeq (x:xs) (y:ys)
  | x < y     = findeq xs (y:ys)
  | x > y     = findeq (x:xs) ys
  | otherwise = x : findeq xs ys

-----------

solution = head (findeq ends starts)

-----------
main :: IO ()
main = print solution
