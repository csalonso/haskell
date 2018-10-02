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
euler2 x = sum [fib i | i <- [0..x], fib i `mod` 2 /= 0, fib i < 4000000]

--3
isPrime :: Int -> Bool
isPrime x = length [x `mod` i | i <- [1..x], x `mod` i == 0] == 2

listPrimes :: Int -> [Int]
listPrimes x = [i | i <- [1..x], isPrime i == True]

euler3 :: Int -> Int
euler3 x = maximum [i | i <- [1..x], isPrime i == True, x `mod` i == 0]
