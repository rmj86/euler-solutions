module EulerUtils.Prime 
  ( primes
  , isPrime
  , primesVec
  , isPrimeMemo
  , isPrimeMemo'
  , sieve
  , sieve'
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (when)

intSqrt :: (Integral a, Integral b) => a -> b
intSqrt = floor . sqrt . fromIntegral

-- List of all primes. Implemented on a standard Haskell list with trial 
-- division.
primes :: [Integer]
primes = 2 : filter p [3,5..]
  where p n = null [d | d<-(takeWhile (<=lim n) primes), mod n d == 0]
        lim n = intSqrt n

-- Primality test. Implemented with trial division on all odds, to avoid memoization. 
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = null [d | d<-(takeWhile (<=lim) ds), mod n d == 0]
  where lim = intSqrt n
        ds = 2:[3,5..]

-------------------------------------------------------------


enumFromStepTo f s t = V.enumFromStepN f s n
     where n = 1 + div (t-f) s

sieve :: Int -> V.Vector Bool
sieve 0 = V.singleton False
sieve n = V.create $ do
    v <- MV.replicate (n+1) True
    MV.write v 0 False
    MV.write v 1 False
    V.forM_ (enumFromStepTo 4 2 n) (\i -> 
        MV.write v i False )
    V.forM_ (enumFromStepTo 3 2 lim) (\p -> do 
        b <- MV.read v p
        when b (
            V.forM_ (enumFromStepTo (p*p) (2*p) n) (\ i-> 
                MV.write v i False ) ) )
    return v
    where lim = intSqrt n

-- sieve' only works on odd values. Index i in the vector represents n=i*2+1.
sieve' :: Int -> V.Vector Bool
sieve' 0 = V.empty
sieve' n = V.create $ do
    v <- MV.replicate (n'+1) True
    MV.write v 0 False
    V.forM_ (V.enumFromN 1 lim) (\i -> do
        b <- MV.read v i
        when b (do
            let p = 2*i+1
            V.forM_ (enumFromStepTo (i+i*p) p n') (\j -> 
                MV.write v j False )))
    return v
    where n' = (n-1) `div` 2
          lim = (intSqrt n - 1) `div` 2


-- Vector of all primes p <= n. 
primesVec :: Int -> V.Vector Int
primesVec n 
  | n < 2     = V.empty
  | otherwise = V.cons 2 . V.map (\i->2*i+1) . V.elemIndices True . sieve' $ n


{- Memoized isPrime. The difference between the two given versions is that,
   isPrimeMemo' has a memo that is faster to generate and more space
   efficient, because it only memoizes odd values. However lookup is slower
   due to the need of calculating the memo index, and the extra guards for
   the even case.
   
  TODO: Test efficiency in action
-}
isPrimeMemo n i = memo V.! i
    where memo = sieve n

isPrimeMemo' n i 
  | i == 2  = True
  | mod i 2 == 0 = False
  | otherwise = memo V.! ((i-1) `div` 2)
    where memo = sieve' n
