module Affine where

size      = 119315717514047
shuffles  = 101741582076661
startA    = 88872671823520
startB    = 72616846730143
endIndex  = 2020
shuffles' = size - shuffles - 1
(a, b)    = getAffine 1 0 startA startB size shuffles'
i         = (a * endIndex + b) `mod` size

getAffine :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
getAffine a b _      _      _    0     = (a, b)
getAffine a b startA startB size times = getAffine a' b' startA startB size times'
  where
    n            = floor(logBase 2 (fromIntegral times))
    times'       = times - (2 ^ n)
    a'           = (newA * a) `mod` size
    b'           = (newB + newA * b) `mod` size
    (newA, newB) = repeatFunction startA startB size n

repeatFunction :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
repeatFunction a b _    0 = (a, b)
repeatFunction a b size n = repeatFunction a' b' size (n - 1)
  where
    a' = (a * a) `mod` size
    b' = (b * (a + 1)) `mod` size
