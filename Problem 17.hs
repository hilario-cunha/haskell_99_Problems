{-|
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example:

* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")
-}
split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split xs n = helper [] xs n
  where 
    helper acc1 (x:xs) 1 = (reverse (x:acc1),xs)
    helper acc1 l@(x:xs) n 
      | n > 0 = helper (x:acc1) xs (n - 1)    
      | otherwise = ([], l)