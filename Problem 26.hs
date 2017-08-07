{-|
(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:

* (combinations 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
Example in Haskell:

> combinations 3 "abcdef"
["abc","abd","abe",...]
-}
import Data.List (tails,subsequences)

combinations :: Eq a => Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

combinations' n xs = filter ((n==).length) (subsequences xs)