{-|
Insert an element at a given position into a list.

Example:

* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"
-}
insertAt :: a -> [a] -> Int -> [a]
insertAt v [] p = [v]
insertAt v xs@(x:xs') p  
  | p <= 1 = v:xs
  | otherwise =  x: insertAt v xs' (p-1)
  
insertAt' :: a -> [a] -> Int -> [a]
insertAt' v xs p = xs' ++ (v:ys')
  where (xs', ys') = splitAt (p-1) xs
