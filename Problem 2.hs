{-|
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}
myButLast :: [a] -> a
myButLast [] = error "No last but one for empty lists!"
myButLast [x] = error "No last but one for singleton lists!"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

myButLast' = head . tail . reverse
myButLast'' = last . init

