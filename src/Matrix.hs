module Matrix where

import           StringMethods
import           Vector

type Matrix = [Vector]

applyTo2dArray :: (a -> b) -> [[a]] -> [[b]]
applyTo2dArray f m = map (map f) m

sumMatrixes :: Matrix -> Matrix -> Matrix
sumMatrixes m1 m2 = operateList sumVectors m1 m2

stringifyMatrix :: Matrix -> String
stringifyMatrix m = joinList "\n" . map stringifyVector $ m

parseStringToMatrix :: String -> Matrix
parseStringToMatrix s = applyTo2dArray read (map (splitString ' ') (splitString '\n' s ))
