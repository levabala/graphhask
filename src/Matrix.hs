module Matrix where

import qualified Data.Vector   as V
import           StringMethods
import qualified Vector2d      as V2d

type Matrix = V2d.Vector2d Float


sumMatrixes :: Matrix -> Matrix -> Matrix
sumMatrixes m1 m2 = V2d.zipWith (+) m1 m2

stringifyMatrix :: Matrix -> String
stringifyMatrix m = joinVector "\n" $ V2d.vector $ V2d.map show m

-- (V.map stringifyVector m)

-- -- parseStringToMatrix :: String -> Matrix
-- -- parseStringToMatrix s = applyTo2dArray read $ U.map (U.fromList . splitString ' ') (U.fromList $ splitString '\n' s )

-- emptyMatrix :: Int -> Int -> Matrix
-- emptyMatrix width height = U.replicate height $ emptyVector width

-- toMutableMatrix :: Matrix ->
