-------------------------------------------------------------------------------
-- Project Euler
-- Problem 99
-------------------------------------------------------------------------------
-- Comparing two numbers written in index form like 2^11 and 3^7 is not
-- difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
-- 
-- However, confirming that 632382^518061 > 519432^525806 would be much more
-- difficult, as both numbers contain over three million digits.
-- 
-- Using eluer99_base_exp.txt, a 22K text file containing one thousand lines
-- with a base/exponent pair on each line, determine which line number has
-- the greatest numerical value.
-- 
-- NOTE: The first two lines in the file represent the numbers in the example
-- given above.
-------------------------------------------------------------------------------
import Data.List (maximumBy)
import Data.Ord (comparing)

-- if   a^b < x^y   then   b * log a < y * log x
-- the latter fits in a Double
evalPair :: (Integer,Integer) -> Double
evalPair (b,e) = fromInteger e * log (fromInteger b)

solution ps = fst . maximumBy (comparing snd) . zip [1..] . map size $ ps
  where size (b,e) = fromInteger e * log (fromInteger b)

------------

getPairs :: IO [(Integer,Integer)]
getPairs = do 
    s <- readFile "euler99_base_exp.txt"
    return $ map (\p-> read ("("++p++")")) . lines $ s

main = print . solution =<< getPairs
