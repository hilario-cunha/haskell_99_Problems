{-|
(*) Remove the K'th element from a list.

Example in Prolog:

?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]
Example in Lisp:

* (remove-at '(a b c d) 2)
(A C D)
(Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

Example in Haskell:

*Main> removeAt 2 "abcd"
('b',"acd")
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt i xs 
  | i <= 0 = (Nothing, xs)
  | otherwise = removeHelper i i [] xs
    where
      removeHelper n 1 acc (x:xs) = (Just x, (reverse acc) ++ xs)
      removeHelper n i acc [] = (Nothing, [])
      removeHelper n i acc (x:xs) = removeHelper n (i-1) (x:acc) xs
  