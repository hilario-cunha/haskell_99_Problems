{-|
Create a list containing all integers within a given range.

Example:

* (range 4 9)
(4 5 6 7 8 9)
Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]
-}
range :: Int -> Int -> [Int]
range begin end 
  | begin > end = []
  | otherwise = helper begin (end-begin)
  where 
    helper i 0 = [i]
    helper i count = i : helper (i+1) (count-1)

range' :: Int -> Int -> [Int]
range' x y = [x..y]

range'' :: Int -> Int -> [Int]
range'' = enumFromTo