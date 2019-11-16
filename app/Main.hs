module Main where

import           Matrix
import           StringMethods
import           Vector


main :: IO ()
main = do
  let input = "1.0 2.0 3.0\n\
  \4.0 5.0 6.0\n\
  \7.0 8.0 9.0"

  let m = parseStringToMatrix input
  let s = stringifyMatrix m
  putStrLn s
