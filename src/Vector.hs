{-# LANGUAGE ParallelListComp #-}

module Vector where

import           StringMethods

type Vector = [Float]

operateList :: (t -> t -> a) -> [t] -> [t] -> [a]
operateList operator v1 v2 = [a `operator` b | a <- v1 | b <- v2]

doWithVectors :: [Float] -> [Float] -> (Float -> Float -> a) -> [a]
doWithVectors v1 v2 operator = operateList operator v1 v2

sumVectors :: Vector -> Vector -> Vector
sumVectors v1 v2 = operateList (+) v1 v2

substractVectors :: Vector -> Vector -> Vector
substractVectors v1 v2 = operateList (-) v1 v2

stringifyVector :: Vector -> String
stringifyVector v = joinList ", " . map show $ v
