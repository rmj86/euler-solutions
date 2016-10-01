--------------------------------------------------------------------------------
-- Project Euler
-- Problem 100
-- Arranged probability
--------------------------------------------------------------------------------
-- If a box contains twenty-one coloured discs, composed of fifteen blue discs
-- and six red discs, and two discs were taken at random, it can be seen that
-- the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
-- 
-- The next such arrangement, for which there is exactly 50% chance of taking
-- two blue discs at random, is a box containing eighty-five blue discs and
-- thirty-five red discs.
-- 
-- By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
-- discs in total, determine the number of blue discs that the box would
-- contain.
--------------------------------------------------------------------------------

vs = (1,0):(1,1):zipWith next vs (tail vs)
  where next (a,b) (x,y) = (a+2*x, b+2*y)

ps = zipWith f vs (tail vs)
  where f (a,b) (x,y) | x^2 < 2*y^2 = (a*x, a*y)
                      | otherwise   = (b*2*y, b*x)

solution = (snd.head) (dropWhile ((<10^12).fst) ps)

------------
main :: IO ()
main = print solution
