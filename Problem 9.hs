{-|
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}
import Data.List

pack :: Eq a => [a] -> [[a]]
pack = reverse . foldl f []
  where f acc x = if((acc /= []) && (head (head acc)) == x) then ((x:(head acc)):(tail acc)) else [x]:acc
  
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : takeWhile (==x) xs) : pack' (dropWhile (==x) xs)

pack'' :: (Eq a) => [a] -> [[a]]
pack'' = Data.List.group

