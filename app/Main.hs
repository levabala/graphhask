module Main where

import Vector
import StringMethods


main :: IO ()
main = do
  let v1 = [1.0, 2.0, 3.0]
  let v2 = [1.0, 2.0, 3.0]
  let v3 = sumV v1 v2 
  let v3String = map show v3
  
  putStrLn (joinList v3String ", ")