{-|
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)
Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.

 data NestedList a = Elem a | List [NestedList a]
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]

-}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List l) = foldl innerFlatten [] l
  where 
    innerFlatten acc (Elem x) = acc ++ [x]
    innerFlatten acc l = acc ++ flatten l

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = flatten' x ++ flatten' (List xs) 

flatten'' :: NestedList a -> [a]
flatten'' (Elem x) = [x]
flatten'' (List x) = concatMap flatten'' x

flatten2 :: NestedList a -> [a]
flatten2 a = flt' a []
  where flt' (Elem x)      xs = x:xs
        flt' (List [])     xs = xs
        flt' (List (x:ls)) xs = flt' x (flt' (List ls) xs)



