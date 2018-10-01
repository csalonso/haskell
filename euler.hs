module Euler where

--1
euler1 :: Int -> Int
euler1 x = sum [i | i <- [0..x-1], i `mod` 3 == 0 || i `mod` 5 == 0]

--2
euler2 :: Int -> Int
