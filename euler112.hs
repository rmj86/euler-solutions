{-------------  Project Euler  -  Problem 112  -  Bouncy numbers  --------------
  Working from left-to-right if no digit is exceeded by the digit to its left
  it is called an increasing number; for example, 134468.
  
  Similarly if no digit is exceeded by the digit to its right it is called a
  decreasing number; for example, 66420.
  
  We shall call a positive integer that is neither increasing nor decreasing a
  "bouncy" number; for example, 155349.
  
  Clearly there cannot be any bouncy numbers below one-hundred, but just over
  half of the numbers below one-thousand (525) are bouncy. In fact, the least
  number for which the proportion of bouncy numbers first reaches 50% is 538.
  
  Surprisingly, bouncy numbers become more and more common and by the time we
  reach 21780 the proportion of bouncy numbers is equal to 90%.
  
  Find the least number for which the proportion of bouncy numbers is
  exactly 99%.
-------------------------------------------------------------------------------}

-- Brute force. Conceptually simple but very slow.

deltas [] = []
deltas xs = zipWith (-) (tail xs) xs

digits 0 = []
digits n = mod n 10 : digits (div n 10)

bouncy n =  any (<0) ds 
         && any (>0) ds
  where ds = deltas . digits $ n

bouncy_proportion = scanl (\(b,t) n-> if bouncy n then (b+1,t+1) else (b,t+1)) 
                          (0,1) [2..]

-- b/t >= 99/100
-- => 100 b >= 99 t
solution = snd . head . filter (\(b,t)-> 100*b >= 99*t) $ bouncy_proportion

{------------------------------------  IO  ------------------------------------}

main = print solution
