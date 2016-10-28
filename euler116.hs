{--------  Project Euler  -  Problem 116  -  Red, green or blue tiles  ---------
  
  A row of five black square tiles is to have a number of its tiles replaced
  with coloured oblong tiles chosen from red (length two), green (length
  three), or blue (length four).
  
  If red tiles are chosen there are exactly seven ways this can be done.
  
  If green tiles are chosen there are three ways.
  
  And if blue tiles are chosen there are two ways.
  
  Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
  replacing the black tiles in a row measuring five units in length.
  
  How many different ways can the black tiles in a row measuring fifty units in
  length be replaced if colours cannot be mixed and at least one coloured tile
  must be used?
  
  NOTE: This is related to Problem 117.
  
  https://projecteuler.net/problem=116
  
-------------------------------------------------------------------------------}

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))

tiling width = f 
    where memo = Map.fromList [(n, f n) | n<-[0..50]]
          f n | n < width = 1
              | otherwise = memo ! (n-1) + memo ! (n-width)

red = tiling 2
green = tiling 3
blue = tiling 4

solution = red 50 + green 50 + blue 50 - 3
--------------------------

tilings' w = ts
  where ts = (replicate w 1) ++ zipWith (+) ts (drop (w-1) ts)

red' = tilings' 2
green' = tilings' 3
blue' = tilings' 4

sol = red'!!50 + green'!!50 + blue'!!50 - 3

main = do
    print solution
    print sol