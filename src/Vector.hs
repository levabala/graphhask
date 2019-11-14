module Vector
    ( Vector,
      sumV
    ) where

type Vector = [Float]

sumV :: Vector -> Vector -> Vector
sumV v1 v2 = zipWith (+) v1 v2