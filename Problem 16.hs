{-|
(**) Drop every N'th element from a list.

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

*Main> dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd $ filter (\(p,_) -> mod p n /= 0) $ zip [1..] xs

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = helper xs n n
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))
