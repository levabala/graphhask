module Main where

import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV


data Matrix a = Matrix {width :: Int, height :: Int, vector :: (V.Vector a)}
data MMatrix s a = MMatrix {widthM :: Int, heightM :: Int, vectorM :: (MV.MVector s a)}

mitem :: Matrix a -> Int -> Int -> a
mitem v x y = (vector v) V.! i
  where
    i = (width v) * y + x

mzipWith :: (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
mzipWith p m1 m2 = m1 {vector= V.zipWith p (vector m1) (vector m2)}

mmap :: (a -> b) -> Matrix a -> Matrix b
mmap f v = v {vector= V.map f (vector v)}

mtranspose :: Matrix a -> Matrix a
mtranspose m = Matrix {width=h, height=w, vector= V.fromList (Prelude.map (\i -> rawVector V.! i) indexes)}
    where
      rawVector = vector m
      pairs = [(i, j) | i <- [0..(width m - 1)], j <- [0..(height m - 1)]]
      w = width m
      h = height m
      indexes = Prelude.map (\(i, j) -> i + j * w) pairs

mgetRowFromIndex :: Matrix a -> Int -> V.Vector a
mgetRowFromIndex m i = V.slice i (width m) (vector m)

mgetRow :: Matrix a -> Int -> V.Vector a
mgetRow m i = V.slice (i * w) w (vector m)
    where
      w = width m

mrows :: Matrix a -> [V.Vector a]
mrows m = result
    where
      accumulator = \acc val -> acc ++ [mgetRowFromIndex (snd val) (fst val)]
      h = height m
      w = width m
      startIndexes = zip (V.toList $ V.enumFromStepN 0 w h) $ replicate h m
      result = foldl accumulator [] startIndexes

mcolumns :: Matrix a -> [V.Vector a]
mcolumns m = mrows $ mtranspose m

mstringifyMatrix :: (Show a) => Matrix a -> String
mstringifyMatrix m = joinList "\n" $ Prelude.map stringifyVector (mrows m)

mlogicalAnd :: Matrix Int -> Matrix Int -> Matrix Int
mlogicalAnd m1 m2 = mzipWith (\val1 val2 -> if (val1 + val2) > 0 then 1 else 0) m1 m2

-- (V.map stringifyVector m)

-- -- parseStringToMatrix :: String -> Matrix
-- -- parseStringToMatrix s = applyTo2dArray read $ U.map (U.fromList . splitString ' ') (U.fromList $ splitString '\n' s )

-- emptyMatrix :: Int -> Int -> Matrix
-- emptyMatrix width height = U.replicate height $ emptyVector width

-- toMutableMatrix :: Matrix ->

accumulateList :: [String] -> String
accumulateList [] = ""
accumulateList (x:xs) = x ++ y
  where y = accumulateList xs

joinList :: String -> [String] -> String
joinList s [] = ""
joinList s [a] = a
joinList separator (x:xs) = a ++ b ++ c
  where
    a = x
    b = separator
    c = joinList separator xs

joinVector :: String -> V.Vector String -> String
joinVector s v = joinList s (V.toList v)

splitStringWithAcc :: Char -> String -> [String] -> [String]
splitStringWithAcc s [] l = l
splitStringWithAcc separator (x:xs) []
  | x == separator = splitStringWithAcc separator xs [[x]]
  | otherwise = splitStringWithAcc separator xs [[x]]

splitStringWithAcc separator (x:xs) accumulator
  | x == separator = splitStringWithAcc separator xs (accumulator ++ [[]])
  | otherwise = splitStringWithAcc separator xs((init accumulator) ++ [(last accumulator) ++ [x]])

splitString :: Char -> String -> [String]
splitString separator string = splitStringWithAcc separator string []
  -- ((init accumulator) ++ [(head accumulator) ++ [x]])
-- splitStringBy :: Char -> String -> [String]
-- splitStringBy separator list (if separator == " " then f1 list else f2 list) : []

stringifyVector :: (Show a) => V.Vector a -> String
stringifyVector v = joinList ", " (V.toList (V.map show v))

countValueInVector :: (Eq a) => V.Vector a -> a -> Int
countValueInVector v value = V.foldl (\count val -> if val == value then count + 1 else count) 0 v

getValueIndexes :: V.Vector Int -> Int -> [Int]
getValueIndexes v value = foldl (\indexes (val, index) -> if val == value then indexes ++ [index] else indexes) [] (zip (V.toList v) ([0..(V.length v)]))

stringifyComponents :: V.Vector Int -> String
stringifyComponents comp = (show (maximalComponent + 1)) ++ "\n" ++ s
    where
      maximalComponent = (V.maximum comp)
      indexes = [0..maximalComponent]
      count = length indexes
      accumulator = (\acc val -> (\valueIndexes -> acc ++ (show $ length valueIndexes) ++ "\n" ++ (joinList " " (map (\v -> show (v + 1)) valueIndexes)) ++ (if val == (count - 1) then "" else "\n")) (getValueIndexes comp val))
      s = foldl accumulator "" indexes

type AdjacencyMatrix = Matrix
type AdjacencyList = [(Int, Int)]

symmetricalСlosure :: Matrix Int -> Matrix Int
symmetricalСlosure m = mlogicalAnd m (mtranspose m)

adjListToMatrix :: (PrimMonad m) => AdjacencyList -> Int -> Int -> MV.MVector (PrimState m) Int -> m()
adjListToMatrix [] w h v = return ()
adjListToMatrix (x:xs) w h v = do
    let whom = fst x
    let toWhom = whom + (snd x) * w

    MV.write v toWhom (1 :: Int)

    adjListToMatrix xs w h v


parseStringToAdjMatrix :: String -> Matrix Int
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

    resultMatrix = Matrix w h filledVector

parseStringToAdjMatrixOrientied :: String -> Matrix Int
parseStringToAdjMatrixOrientied s = symmetricalСlosure $ parseStringToAdjMatrix s

comp_dfs :: (PrimMonad m) =>
  Int   -- component index
  -> Int -- graph index
  -> Matrix  Int -- graph adjacency matrix
  -> MV.MVector (PrimState m) Int -- used vector
  -> MV.MVector (PrimState m) Int -- components vector
  -> m()

comp_dfs i v m used comp = do
  MV.write used v 1
  MV.write comp v i


  -- putStrLn ("vertex: " ++ (show v))

  let indexes = V.fromList [0..(width m - 1)]

  forM_ (V.zip indexes (mgetRow m v)) (\(u, a) -> do
    us <- MV.read used u

    -- usedI <- V.freeze used
    -- print usedI

    if a == 1 && not (us == 1)
      then
        comp_dfs i u m used comp
      else
        return ()
      )

comp_internal :: (PrimMonad m) => MV.MVector (PrimState m) Int -> MV.MVector (PrimState m) Int -> Matrix Int -> m Int -> Int -> m Int
comp_internal used comp m compI v   = do
  compM <- compI
  us <- MV.read used v

  -- putStrLn ("find component for vertex: " ++ (show v) ++ (if us == 0 then " (not used)" else " (used)"))

  if us == 0
    then do
      comp_dfs compM v m used comp

      -- putStrLn ("compM increased: " ++ (show compM))

      return (compM + 1)
    else
      return compM


comp :: (PrimMonad m) => Matrix Int -> MV.MVector (PrimState m) Int -> MV.MVector (PrimState m) Int -> m Int
comp m used comp = do
  let indexes = [0..((width m) - 1)]
  foldl (comp_internal used comp m) (return 0) indexes


main :: IO()
main = do
  -- let s = "6 4\n\
  -- \3 1\n\
  -- \1 2\n\
  -- \5 4\n\
  -- \2 3"
  metaS <- getLine
  let metaSChunks = splitString ' ' metaS
  -- let N = read (metaSChunks !! 0)
  let adjCount = read (metaSChunks !! 1)

  lines <- replicateM adjCount getLine
  let s = metaS ++ (joinList "\n" (foldl (\acc val -> acc ++ [val]) [] lines))

  let m = parseStringToAdjMatrixOrientied s
  -- putStrLn $ mstringifyMatrix m

  used <- MV.replicate 6 0
  components <- MV.replicate 6 0

  comp m used components
  componentsI <- V.freeze components
  -- usedI <- V.freeze used

  -- print compI
  -- print usedI

  putStrLn $ stringifyComponents componentsI
