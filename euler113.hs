{-----------  Project Euler  -  Problem 113  -  Non-bouncy numbers  ------------
  Working from left-to-right if no digit is exceeded by the digit to its left
  it is called an increasing number; for example, 134468.
  
  Similarly if no digit is exceeded by the digit to its right it is called a
  decreasing number; for example, 66420.
  
  We shall call a positive integer that is neither increasing nor decreasing
  a "bouncy" number; for example, 155349.
  
  As n increases, the proportion of bouncy numbers below n increases such that
  there are only 12951 numbers below one-million that are not bouncy and only
  277032 non-bouncy numbers below 10^10.
  
  How many numbers below a googol (10^100) are not bouncy?
-------------------------------------------------------------------------------}

snb n k = div (f (n+k-1)) (f n * f (k-1))
  where f n = product [1..n]

solution = inc + dec - const - zero
  where inc = snb n 10
        dec = snb n 11
        const = 9 * n
        zero = 1 + n+1
        n = 100

---------

main = print solution
