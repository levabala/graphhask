{-# LANGUAGE ParallelListComp #-}

module Vector where

import qualified Data.Vector   as Vector
import           StringMethods

type VectorF = Vector.Vector Float

stringifyVector :: VectorF -> String
stringifyVector v = joinVector " " ( Vector.map show v)

emptyVector :: Int -> VectorF
emptyVector l = Vector.replicate l 0
