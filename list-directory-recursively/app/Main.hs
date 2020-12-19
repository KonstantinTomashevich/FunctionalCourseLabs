module Main where

import Lib ( listFilesRecursively )
import System.IO.Unsafe ( unsafePerformIO )

main :: IO ()
main = do
    putStrLn "Input directory path:"
    dir <- getLine
    putStrLn "Input filter:"
    filter <- getLine

    -- unsafePerformIO is used only to make print code more readable and clear (otherwise it's a mess).
    mapM_ putStrLn (unsafePerformIO $ listFilesRecursively dir filter)
    putStrLn "Done!"
