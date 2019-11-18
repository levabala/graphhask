module Graph where

-- import qualified Data.Map                    as Map
import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Bool               as B
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import qualified Matrix                  as M
import           StringMethods

type AdjacencyMatrix = M.Matrix
type AdjacencyList = [(Int, Int)]

symmetricalСlosure :: M.Matrix Int -> M.Matrix Int
symmetricalСlosure m = M.logicalAnd m (M.transpose m)

adjListToMatrix :: (PrimMonad m) => AdjacencyList -> Int -> Int -> MV.MVector (PrimState m) Int -> m()
adjListToMatrix [] w h v = return ()
adjListToMatrix (x:xs) w h v = do
    let whom = fst x
    let toWhom = whom + (snd x) * w

    MV.write v toWhom (1 :: Int)

    adjListToMatrix xs w h v


parseStringToAdjMatrix :: String -> M.Matrix Int
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
        listToTuple = \line -> (read (line !! 0) - 1, read (line !! 1) - 1)
        splittedLines = map (splitString ' ') adjListString

    emptyVector = V.replicate (w * h) (0 :: Int)
    filledVector = V.modify (adjListToMatrix adjList w h) emptyVector

    resultMatrix = M.Matrix w h filledVector

parseStringToAdjMatrixOrientied :: String -> M.Matrix Int
parseStringToAdjMatrixOrientied s = symmetricalСlosure $ parseStringToAdjMatrix s

dfs :: (PrimMonad m) =>
  Int   -- component index
  -> Int -- graph index
  -> M.Matrix  Int -- graph adjacency matrix
  -> MV.MVector (PrimState m) Bool -- used vector
  -> MV.MVector (PrimState m) Int -- components vector
  -> m()

dfs i v m used comp = do
  MV.write used v True
  MV.write comp v i

  let indexes = V.fromList [1..(M.width m)]
  forM_ (V.zip indexes (M.getRow m v)) (\(u, a) -> if a == 1 && not B.Extras.mwhen (MV.read used u) then dfs u v m used comp else return ())

