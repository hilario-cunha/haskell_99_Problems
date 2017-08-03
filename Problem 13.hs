{-|
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
data EncodedData a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [EncodedData a]
encodeDirect = reverse . encodeHelper []
  where 
    encodeHelper acc [] = acc
    encodeHelper acc xs@(x:_) = 
      let 
        (xList, rest) = span (x==) xs
        newAcc = (toEncodedData x (length xList)):acc
        result = encodeHelper newAcc rest
      in result
    toEncodedData x len = if(len == 1) then Single x else Multiple len x
      
  

