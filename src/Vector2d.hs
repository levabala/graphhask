module Vector2d where

import           Data.Vector
import qualified Data.Vector as V

data Vector2d a = Vector2d {width :: Int, height :: Int, vector :: (Vector a)}

item :: Vector2d a -> Int -> Int -> a
item v x y = (vector v) ! i
  where
    i = (width v) * y + x

zipWith :: (a -> a -> a) -> Vector2d a -> Vector2d a -> Vector2d a
zipWith p v1 v2 = v1 {vector= V.zipWith p (vector v1) (vector v2)}

map :: (a -> b) -> Vector2d a -> Vector2d b
map f v = v {vector= V.map f (vector v)}
