{-----  Project Euler  -  Problem 115  -  Counting block combinations II  ------
  
  A row measuring n units in length has red blocks with a minimum length of m
  units placed on it, such that any two red blocks (which are allowed to be
  different lengths) are separated by at least one black square.
  
  Let the fill-count function, F(m, n), represent the number of ways that a row
  can be filled.
  
  For example, F(3, 29) = 673135 and F(3, 30) = 1089155.
  
  That is, for m = 3, it can be seen that n = 30 is the smallest value for
  which the fill-count function first exceeds one million.
  
  In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
  F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
  function first exceeds one million.
  
  For m = 50, find the least value of n for which the fill-count function first
  exceeds one million.
  
  NOTE: This is a more difficult version of Problem 114.
  
  https://projecteuler.net/problem=115
  
-------------------------------------------------------------------------------}

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))

fillCount m = f
    where
    f n | n < m     = 1
        | otherwise = memo ! n
    memo = Map.fromList [ (n,  f (n-1) + sum [f i | i <- [-1..n-m-1]] )
                        | n <- [0..1000] ]

solution = fst . head . dropWhile ((<1000000).snd) $ [(n , f n) | n<-[0..]]
    where f = fillCount 50

main = print solution