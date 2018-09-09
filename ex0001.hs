module Ex0001 where

-- |Find the sum of all the multiples of x or y below n given their least common multiple xy.
sumOfMultiplesBelow :: Integral a => a -> a -> a -> a -> a
sumOfMultiplesBelow n x y xy = (sumOfMultiples' x) + (sumOfMultiples' y) - (sumOfMultiples' xy)
  where
    multiples z = div (n-1) z
    count m = div (m * (m+1)) 2
    sumOfMultiples' z = z * (count (multiples z))

solution :: Integral a => a
solution = sumOfMultiplesBelow 1000 3 5 15
