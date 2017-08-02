{-|
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
import Data.List

data EncodedData a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodedData a]
encodeModified = map mapFunc . group
  where 
    mapFunc l = if (length l == 1) 
          then Single (head l) 
          else Multiple (length l) (head l)

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\l -> (length l,head l)) . group

encodeModified' :: (Eq a) => [a] -> [EncodedData a]
encodeModified' =  map mapFunc . encode 
  where 
    mapFunc (1,x) = Single x
    mapFunc (n,x) = Multiple n x
