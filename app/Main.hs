module Main where

import qualified Data.Vector   as V
import qualified Graph         as G
import qualified Matrix        as M
import           StringMethods


main :: IO()
main = do
  -- let m = M.Matrix 2 3 (V.fromList [0..6])
  -- let s = M.stringifyMatrix m
  -- putStrLn s
  let s = "6 4\n\
  \3 1\n\
  \1 2\n\
  \5 4\n\
  \2 3"
  let m = G.parseStringToAdjMatrix s
  putStrLn M.stringifyMatrix m
