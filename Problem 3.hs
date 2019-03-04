{-|
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}
elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs i 
  | length xs < i = error "Index out of bounds"
  | i < 1 = error "Index out of bounds"
  | otherwise = xs !! (i - 1)

elementAt'' :: [a] -> Int -> a
elementAt'' [] i = error "Index out of bounds"
elementAt'' (x:_) 1 = x
elementAt'' (x:xs) n = elementAt'' xs (n-1)
