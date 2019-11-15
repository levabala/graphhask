module StringMethods where


import           Data.Char

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
