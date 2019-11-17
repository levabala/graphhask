module Graph where

-- import qualified Data.Map                    as Map
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import qualified Matrix                  as M
import           StringMethods


type AdjacencyMatrix = M.Matrix
type AdjacencyList = [(Int, Int)]

adjListToMatrix :: AdjacencyList -> Int -> Int -> M.MMatrix s Float
adjListToMatrix l w h = do
    let m = M.MMatrix w h (MV.replicate (w * h) 0)
    let v = M.vectorM m
    MV.write v 1 1

    M.Matrix w h (V.freeze (M.vectorM m))


parseStringToAdjMatrix :: (PrimMonad m) => String -> M.Matrix Float -> m()
parseStringToAdjMatrix s = resultMatrix
  where
    lines = splitString '\n' s
    w = read $ firstLineSplitted !! 0
      where
        firstLineSplitted = splitString ' ' $ lines !! 0

    h = w

    adjListString = tail lines
    adjList = map listToTuple splittedLines
      where
        listToTuple = \line -> (line !! 0, line !! 1)
        splittedLines = map (splitString ' ') adjListString

    resultMatrix = adjListToMatrix adjList w h

