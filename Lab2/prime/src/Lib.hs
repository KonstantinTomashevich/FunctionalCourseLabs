module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = print $ primes !! 5

primes :: [[Integer]]
primes = [ firstNPrimes n | n <- [0 .. ]]

firstNPrimes :: Integer -> [Integer]
firstNPrimes count
  | count == 0 = []
  | otherwise = appendPrime $ firstNPrimes (count - 1)

appendPrime :: [Integer] -> [Integer] 
appendPrime known
  | null known = [2]
  | otherwise = head [ n | n <- [head known ..], null [ divider | divider <- known, mod n divider == 0]] : known