{-------------------------------------------------------------------------------
  Truncatable primes
  Problem 37
  
  The number 3797 has an interesting property. Being prime itself, it is
  possible to continuously remove digits from left to right, and remain prime
  at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
  left: 3797, 379, 37, and 3.
  
  Find the sum of the only eleven primes that are both truncatable from left
  to right and right to left.
  
  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  
  https://projecteuler.net/problem=37
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Algorithm:
  1: recursively generate all left-truncable primes
  2: filter for the ones which are right-truncable
  
  It's easy to generate left-truncable (lt) primes. Given a lt prime p,
  let p' = 10*p+d. if p' is prime, then it is also lt, because all it's
  truncations also belong to p. for p' to be prime, d has to be in [1,3,7,9].
  If the lt's are finite, step two is sufficient. If the lt's are infinite, 
  more work is required to prove whether we have all truncable primes.
  Luckily, the lt's are finite.
-------------------------------------------------------------------------------}

import EulerUtils.Prime (isPrime)

generateLT [] = []
generateLT seeds = next ++ generateLT next
    where
    next = [ p'
           | p <- seeds
           , d <- [1,3,7,9]
           , let p' = 10*p+d
           , isPrime p' ]
           
lts = generateLT [2,3,5,7]

truncRight n = takeWhile (<n) [ mod n (10^i)
                              | i <- [1..] ]

truncables = [ p
             | p <- lts
             , all isPrime (truncRight p) ]

-- IO

solution = sum truncables

main = print solution
