module Main where

import           Matrix
import           StringMethods
import           Vector


main :: IO ()
main = do
  let input = "1.0 2.0 3.0\
  \4.0 5.0 6.0\
  \7.0 8.0 9.0"

  let m = parseStringToMatrix input
  let s = stringifyMatrix m
  putStrLn s
  -- let arr = splitString ' ' "abc qwe s s"
  -- putStrLn $ joinList ", " arr

  -- let v1 = [1.0, 2.0, 3.0]
  -- let v2 = [1.0, 2.0, -3.0]
  -- let v3 = sumVectors v1 v2

  -- putStrLn (stringifyVector v3)

  -- let m1 = [ [1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0] ]
  -- let m2 = [ [5.0, 5.0, 3.0], [3.0, 3.0, 5.0], [5.0, 5.0, 3.0] ]
  -- let m3 = sumMatrixes m1 m2

  -- putStrLn $ stringifyMatrix m1
