-- exercise 1 
-- my implemenatation of some build-in (prelude) functions

module Main where

mySum :: (Num a) => [a] -> a
mySum list = foldl (\acc val -> acc + val) 0 list

myProduct :: (Num a) => [a] -> a
myProduct list = foldl (\acc val -> acc * val ) 1 list

myReverse :: [a] -> [a]
myReverse list = foldl (\acc val -> val : acc) [] list

myAnd :: [Bool] -> Bool
myAnd list = foldl (\acc val -> acc && val)True list

myOr :: [Bool] -> Bool
myOr list = foldl (\acc val -> acc || val)False list

myHead :: [a] -> a
myHead = foldr(\a b -> a ) undefined

myLast :: [a] -> a
myLast = foldl(\a b -> b ) undefined

list = [1,2,3,4,5]

main = do
    print("[1,2,3,4,5]")
    print("mySum result: ", mySum list)
    print("myProduct result: ", myProduct list)
    print("myReverse result: ",myReverse list)
    print("myAnd [True,False,True] result: ", myAnd [True,False,True])
    print("myOr  [True,False,True] result: ",myOr [True,False,True])
    print("myHead result: ",myHead list)
    print("myLast result: ",myLast list)