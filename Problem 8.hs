{-|
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
Example in Haskell:

> compress "aaaabccaadeeee"
"abcade"
-}
import Data.List

compress :: Eq a => [a] -> [a]
compress xs = foldl f [] xs
  where f acc x = if ( (acc /= []) && (last acc) == x) then acc else (acc ++ [x])

compress' :: Eq a => [a] -> [a]
compress' = reverse . (foldl f [])
  where 
  f acc x = if ( (acc /= []) && (head acc) == x) then acc else (x:acc)


compress'' :: Eq a => [a] -> [a]
compress'' = (map head) . group






