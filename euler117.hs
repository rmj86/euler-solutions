{-------  Project Euler  -  Problem 117  -  Red, green, and blue tiles  --------
https://projecteuler.net/problem=117

Using a combination of black square tiles and oblong tiles chosen from: red
tiles measuring two units, green tiles measuring three units, and blue tiles
measuring four units, it is possible to tile a row measuring five units in
length in exactly fifteen different ways.

How many ways can a row measuring fifty units in length be tiled?

NOTE: This is related to Problem 116.
-------------------------------------------------------------------------------}
import Data.List (tails, transpose)

tilings = 1:1:2:4:(map sum . transpose . take 4 . tails $ tilings)

-------------------
main = print (tilings !! 50)
