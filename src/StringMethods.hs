module StringMethods where

import           Data.Char
import qualified Data.Vector as V

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
