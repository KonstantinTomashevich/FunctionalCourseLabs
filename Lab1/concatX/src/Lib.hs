module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ concatX [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

concatX :: [[a]] -> [a]
concatX [] = []
concatX array = head array ++ concatX (tail array)