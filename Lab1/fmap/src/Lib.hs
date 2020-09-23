module Lib
    ( someFunc,
      myMap
    ) where

someFunc :: IO ()
someFunc = print $ myMap (subtract 1) [1, 2, 3, 4, 5, 6, 7, 8, 9]

myMap :: (a -> b) -> [a] -> [b]
myMap converter [] = []
myMap converter (listHead:listTail) = converter listHead : myMap converter listTail