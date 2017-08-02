{-|
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}
data EncodedData a = Single a | Multiple Int a deriving (Show)

decodeModified :: (Eq a) => [EncodedData a] -> [a]
decodeModified = concatMap decodeHelper
  where 
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x
