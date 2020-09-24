module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = print $ tail $ head $ dropWhile (\array -> head array < 1000) firstNPrimeArrays

firstNPrimes :: Integer -> [Integer]
firstNPrimes count
  | count == 0 = []
  | otherwise = appendPrime $ firstNPrimes (count - 1)

-- Собрать бесконечный список из итеративного вариант решета 
-- Эратосфена (который немного эффективнее) так и не получилось.
-- Получилась только такая "химера" на iterate.
firstNPrimeArrays :: [[Integer]]
firstNPrimeArrays = tail $ iterate (appendPrime) []

appendPrime :: [Integer] -> [Integer] 
appendPrime known
  | null known = [2]
  | otherwise = head [ n | n <- [head known ..], null [ divider | divider <- known, mod n divider == 0]] : known

-- Самый простой, но адски неэффективный вариант.
verySlowPrimes :: [Integer]
verySlowPrimes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [ n | n <- xs, mod n p > 0]