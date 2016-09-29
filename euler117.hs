{-------  Project Euler  -  Problem 117  -  Red, green, and blue tiles  --------
https://projecteuler.net/problem=117

Using a combination of black square tiles and oblong tiles chosen from: red
tiles measuring two units, green tiles measuring three units, and blue tiles
measuring four units, it is possible to tile a row measuring five units in
length in exactly fifteen different ways.

How many ways can a row measuring fifty units in length be tiled?

NOTE: This is related to Problem 116.
-------------------------------------------------------------------------------}

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))

memo = Map.fromList [(n,tilings n) | n<-[0..50]]

tilings 0 = 1
tilings 1 = 1
tilings 2 = 2
tilings 3 = 4
tilings n = memo!(n-1) + memo!(n-2) + memo!(n-3) + memo!(n-4)

solution = tilings 50

main = print solution