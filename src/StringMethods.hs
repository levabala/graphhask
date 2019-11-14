module StringMethods (accumulateList, joinList) where

accumulateList :: [String] -> String
accumulateList [] = ""
accumulateList (x:xs) = x ++ y
  where y = accumulateList xs

joinList :: [String] -> String -> String
joinList [] s = ""
joinList [a] s = a
joinList (x:xs) separator = a ++ b ++ c
  where 
    a = x
    b = separator
    c = joinList xs separator