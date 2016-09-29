{------  Project Euler  -  Problem 114  -  Counting block combinations I  ------

  A row measuring seven units in length has red blocks with a minimum length of
  three units placed on it, such that any two red blocks (which are allowed to
  be different lengths) are separated by at least one black square. There are
  exactly seventeen ways of doing this.

  How many ways can a row measuring fifty units in length be filled?

  NOTE: Although the example above does not lend itself to the possibility, in
  general it is permitted to mix block sizes. For example, on a row measuring
  eight units in length you could use red (3), black (1), and red (4).
  
  https://projecteuler.net/problem=114
  
-------------------------------------------------------------------------------}

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))

memo = Map.fromList [(n, tilings n) | n<-[-1..50]]

tilings n 
 | n < 3 = 1
 | otherwise = memo!(n-1) + sum [memo!i | i<-[(-1)..n-4]]
 
solution = tilings 50

main = print solution