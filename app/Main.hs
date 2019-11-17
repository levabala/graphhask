module Main where

import qualified Data.Vector.Unboxed as U
import           Matrix
import           StringMethods
import           Vector2d


main :: IO()
main = do
  let v = Vector2d {width=5, height=2, vector=U.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}
  let el = item v 4 1
  putStrLn $ show el
