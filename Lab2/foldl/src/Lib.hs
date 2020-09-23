module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ foldl (/) 64 [4, 2, 4]

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl function argument queue
    | null queue = argument
    | otherwise = myFoldl function (function argument (head queue)) (tail queue)