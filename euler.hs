module Euler where

--1
euler1 :: Int -> Int
euler1 x = sum [i | i <- [0..x-1], i `mod` 3 == 0 || i `mod` 5 == 0]

--2
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

euler2 :: Int -> Int
euler2 x = sum [fib i | i <- [0..x], fib i `mod` 2 /= 0]
