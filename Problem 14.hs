{-|
(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:

> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}
dupli :: [a] -> [a]
dupli = foldl (\acc x -> acc ++ [x,x]) []

dupli' :: [a] -> [a]
dupli' = foldr (\x xs -> x : x : xs) []

dupli'' :: [a] -> [a]
dupli'' [] = []
dupli'' (x:xs) = x:x:(dupli' xs)

