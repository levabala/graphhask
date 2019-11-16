module Graph where

import qualified Data.Map            as Map
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           Matrix
import           StringMethods


type AdjacencyMatrix = Matrix
type AdjacencyList = [(Int, Int)]

-- applyAdjListToMatrix :: AdjacencyList -> Matrix -> Matrix
-- applyAdjListToMatrix l m =

parseStringToAdjMatrix :: String -> Matrix
parseStringToAdjMatrix s = resultMatrix
  where
    lines = splitString '\n' s
    width = read $ firstLineSplitted !! 0
      where
        firstLineSplitted = splitString ' ' $ lines !! 0

    height = width

    adjListString = tail lines
    adjList = map listToTuple splittedLines
      where
        listToTuple = \line -> (line !! 0, line !! 1)
        splittedLines = map (splitString ' ') adjListString


