{-|
  (*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' = head . reverse

myLast'' [] = error "No end for empty lists!"  
myLast'' x = x !! (length x -1)