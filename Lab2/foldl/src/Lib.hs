module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ foldl (/) 64 [4, 2, 4]

-- foldl значительно быстрее foldr, так как foldl не тратит время на итерацию по 
-- всему списку для получения второго аргумента, а берёт его голову как аргумент.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl function argument queue
    | null queue = argument
    | otherwise = myFoldl function (function argument (head queue)) (tail queue)