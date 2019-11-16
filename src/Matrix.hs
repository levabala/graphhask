module Matrix where

import qualified Data.Vector   as Vector
import           StringMethods
import           Vector

type Matrix = Vector.Vector VectorF

applyTo2dArray :: (a -> b) -> Vector.Vector (Vector.Vector a) -> Vector.Vector (Vector.Vector b)
applyTo2dArray f m = Vector.map (Vector.map f) m

sumMatrixes :: Matrix -> Matrix -> Matrix
sumMatrixes m1 m2 = Vector.zipWith (Vector.zipWith (+)) m1 m2

stringifyMatrix :: Matrix -> String
stringifyMatrix m = joinVector "\n" $ Vector.map stringifyVector $ m

parseStringToMatrix :: String -> Matrix
parseStringToMatrix s = applyTo2dArray read $ Vector.map (Vector.fromList . splitString ' ') (Vector.fromList $ splitString '\n' s )

emptyMatrix :: Int -> Int -> Matrix
emptyMatrix width height = Vector.replicate height $ emptyVector width

-- toMutableMatrix :: Matrix ->
