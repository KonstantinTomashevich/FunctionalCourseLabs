module Lib
  ( someFunc
  , maclaurinSin
  ) where

someFunc :: IO ()
someFunc = print $ maclaurinSin (pi / 3) 1000

maclaurinSin :: Float -> Integer -> Float
maclaurinSinStep :: Float -> Float -> Integer -> Integer -> Float -> Float

maclaurinSin argument iterations = maclaurinSinStep argument 0.0 (iterations - 1) 1 argument

maclaurinSinStep initialArgument resultAccumulator iterationsLeft dividerIterator lastResult
  | iterationsLeft == 0 = resultAccumulator + lastResult
  | otherwise =
    maclaurinSinStep
      initialArgument
      (resultAccumulator + lastResult)
      (iterationsLeft - 1)
      (dividerIterator + 2)
      (-1.0 * lastResult * initialArgument * initialArgument /
        fromInteger ((dividerIterator + 1) * (dividerIterator + 2)))
