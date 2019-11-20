module Main where

import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import qualified Graph                   as G
import qualified Matrix                  as M
import           StringMethods

main :: IO()
main = do
  -- let s = "6 4\n\
  -- \3 1\n\
  -- \1 2\n\
  -- \5 4\n\
  -- \2 3"
  metaS <- getLine
  let metaSChunks = splitString ' ' metaS
  -- let N = read (metaSChunks !! 0)
  let adjCount = read (metaSChunks !! 1)

  lines <- replicateM adjCount getLine
  let s = metaS ++ (joinList "\n" (foldl (\acc val -> acc ++ [val]) [] lines))

  let m = G.parseStringToAdjMatrixOrientied s
  -- putStrLn $ M.stringifyMatrix m

  used <- MV.replicate 6 0
  comp <- MV.replicate 6 0

  G.comp m used comp
  compI <- V.freeze comp
  -- usedI <- V.freeze used

  -- print compI
  -- print usedI

  putStrLn $ stringifyComponents compI
