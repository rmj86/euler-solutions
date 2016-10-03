--------------------------------------------------------------------------------
-- Large non-Mersenne prime
-- Problem 97
--------------------------------------------------------------------------------
-- The first known prime found to exceed one million digits was discovered in 
-- 1999, and is a Mersenne prime of the form 2^6972593-1; it contains exactly 
-- 2,098,960 digits. Subsequently other Mersenne primes, of the form 2p-1, have 
-- been found which contain more digits.
-- 
-- However, in 2004 there was found a massive non-Mersenne prime which contains 
-- 2,357,207 digits: 28433*2^7830457+1.
-- 
-- Find the last ten digits of this prime number.
--------------------------------------------------------------------------------

-- easy but slow
-- solution = mod (28433*2^7830457+1) (10^10)

powmod a n b 
  | n==0   = 1
  | even n = mod x b
  | odd n  = mod (a*x) b
  where x = powmod a' n' b
        a' = mod (a*a) b
        n' = div n 2

solution = mod (28433 * powmod 2 7830457 b + 1) b
  where b = 10^10

--------------------------------------------------------------------------------

main = print solution
