module Euler where

soma :: Int -> Int
soma x = sum [i | i <- [0..x-1], i `mod` 3 == 0 || i `mod` 5 == 0]
