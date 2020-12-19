module Main where

import Lib
import Data.Map ( fromList )

commands = do
    load 0;
    write 1;
    load 3;
    negt;
    Lib.div 1;

main :: IO ()
main = putStrLn $ "Result: " ++ (show $ runCommands commands (fromList [(0, 0)]))
