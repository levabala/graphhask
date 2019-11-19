module Graph where

-- import qualified Data.Map                    as Map
import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
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

-- comp_dfs :: (PrimMonad m) =>
--   Int   -- component index
--   -> Int -- graph index
--   -> M.Matrix  Int -- graph adjacency matrix
--   -> MV.MVector (PrimState m) Int -- used vector
--   -> MV.MVector (PrimState m) Int -- components vector
--   -> m()

comp_dfs i v m used comp = do
  MV.write used v 1
  MV.write comp v i


  putStrLn ("vertex: " ++ (show v))

  let indexes = V.fromList [0..(M.width m - 1)]

  forM_ (V.zip indexes (M.getRow m v)) (\(u, a) -> do
    us <- MV.read used u

    -- usedI <- V.freeze used
    -- print usedI

    if a == 1 && not (us == 1)
      then
        comp_dfs i u m used comp
      else
        return ()
      )

-- comp_internal :: (PrimMonad m) => MV.MVector (PrimState m) Int -> MV.MVector (PrimState m) Int -> M.Matrix Int -> m Int -> Int -> m Int
comp_internal used comp m compI v   = do
  compM <- compI
  us <- MV.read used v

  putStrLn ("find component for vertex: " ++ (show v) ++ (if us == 0 then " (not used)" else " (used)"))

  if us == 0
    then do
      comp_dfs compM v m used comp

      putStrLn ("compM increased: " ++ (show compM))

      return (compM + 1)
    else
      return compM


-- comp :: (PrimMonad m) => M.Matrix Int -> MV.MVector (PrimState m) Int -> MV.MVector (PrimState m) Int -> m Int
comp m used comp = do
  let indexes = [0..((M.width m) - 1)]
  foldl (comp_internal used comp m) (return 0) indexes
