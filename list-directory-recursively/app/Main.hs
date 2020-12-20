module Main where

import Lib ( listFilesRecursively )

main :: IO ()
main = do
    putStrLn "Input directory path:"
    dir <- getLine
    putStrLn "Input filter:"
    filter <- getLine

    let files = listFilesRecursively dir filter
    mapM_ putStrLn =<< files
    putStrLn "Done!"
