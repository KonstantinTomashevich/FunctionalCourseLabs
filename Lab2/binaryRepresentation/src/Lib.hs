module Lib
    ( someFunc
    ) where

import Data.Bits

someFunc :: IO ()
someFunc = print $ binaryRepresentation $ -1337

binaryRepresentation :: Int -> [Char] 
binaryRepresentation input = [ if testBit input n then '1' else '0' | n <- [(finiteBitSize input) - 1, (finiteBitSize input) - 2 .. 0]]
