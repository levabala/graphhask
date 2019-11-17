module Matrix where

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           StringMethods

data Matrix a = Matrix {width :: Int, height :: Int, vector :: (V.Vector a)}
data MMatrix s a = MMatrix {widthM :: Int, heightM :: Int, vectorM :: (MV.MVector s a)}

item :: Matrix a -> Int -> Int -> a
item v x y = (vector v) V.! i
  where
    i = (width v) * y + x

zipWith :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
zipWith p v1 v2 = v1 {vector= V.zipWith p (vector v1) (vector v2)}

map :: (a -> b) -> Matrix a -> Matrix b
map f v = v {vector= V.map f (vector v)}

transpose :: Matrix a -> Matrix a
transpose m = Matrix {width=h, height=w, vector= V.fromList (Prelude.map (\i -> rawVector V.! i) indexes)}
    where
      rawVector = vector m
      pairs = [(i, j) | i <- [0..(width m - 1)], j <- [0..(height m - 1)]]
      w = width m
      h = height m
      indexes = Prelude.map (\(i, j) -> i + j * w) pairs

getRowFromIndex :: Matrix a -> Int -> V.Vector a
getRowFromIndex m i = V.slice i (width m) (vector m)

rows :: Matrix a -> [V.Vector a]
rows m = result
    where
      accumulator = \acc val -> acc ++ [getRowFromIndex (snd val) (fst val)]
      h = height m
      w = width m
      startIndexes = zip (V.toList $ V.enumFromStepN 0 w h) $ replicate h m
      result = foldl accumulator [] startIndexes

columns :: Matrix a -> [V.Vector a]
columns m = rows $ transpose m

stringifyMatrix :: (Show a) => Matrix a -> String
stringifyMatrix m = joinList "\n" $ Prelude.map stringifyVector (rows m)

-- (V.map stringifyVector m)

-- -- parseStringToMatrix :: String -> Matrix
-- -- parseStringToMatrix s = applyTo2dArray read $ U.map (U.fromList . splitString ' ') (U.fromList $ splitString '\n' s )

-- emptyMatrix :: Int -> Int -> Matrix
-- emptyMatrix width height = U.replicate height $ emptyVector width

-- toMutableMatrix :: Matrix ->
