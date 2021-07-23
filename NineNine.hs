-- All below problems and their solutions can be found at https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems.
-- Below solutions are my (beginner's) solution to these problems. - https://github.com/prabhat-g97
-- Feel free to fork this repository.
-- Please note that I won't be merging any Pull requests. However, corrections are welcomed.
-- I will be adding better solutions as I learn more. Happy Coding!

module NineNine where

import Control.Monad ( liftM2 )
import Data.List (foldl')

-------------------------------------------------------------------------------- Problems 1 - 10

-- *** PROBLEM 1: Find the last element of a list.***

-- Crashing for empty list input
myLast :: [a] -> a
myLast []       = error "List is empty"
myLast [x]      = x
myLast (x : xs) = myLast xs

-- Using maybe to avoid crashing
myLast' :: [a] -> Maybe a
myLast' []       = Nothing
myLast' [x]      = Just x
myLast' (x : xs) = myLast' xs

-- *** PROBLEM 2: Find the last but one element of a list.***

-- Crashing for empty list input
myButlast :: [a] -> a
myButlast []       = error "List is empty"
myButlast [x]      = error "Insufficient elements in the list"
myButlast [x,y]    = x
myButlast (x : xs) = myButlast xs

-- Using maybe to avoid crashing
myButLast' :: [a] -> Maybe a
myButLast' []       = Nothing
myButLast' [x]      = Nothing
myButLast' [x,y]    = Just x
myButLast' (x : xs) = myButLast' xs

-- *** PROBLEM 3: Find the K'th element of a list. The first element in the list is number 1.***

elementAt :: [a] -> Int -> a
elementAt []       n = error "Insufficient elements in the list"
elementAt (x : xs) 1 = x
elementAt (x : xs) n
    | n<1            = error "Position cannot be less than 1"
    | otherwise      = elementAt xs (n-1)

-- *** PROBLEM 4: Find the number of elements of a list.***

-- Without foldr
myLength :: [a] -> Int
myLength []       = 0
myLength (x : xs) = 1 + myLength xs

-- Using foldr
myLength' :: [a] -> Int
myLength' = foldr (\x r-> 1 + r) 0

-- *** PROBLEM 5: Reverse a list.***

-- Without accumulator
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = reverse xs ++ [x]

-- With accumulator
myReverse' :: [a] -> [a]
myReverse' = myReverseAux []

myReverseAux :: [a] -> [a] -> [a]
myReverseAux acc []       = acc
myReverseAux acc (x : xs) = myReverseAux (x : acc) xs

-- Using foldr
myReverse'' :: [a] -> [a]
myReverse'' = foldr (\x acc -> acc ++ [x]) []

-- using foldl'
myReverse''' :: [a] -> [a]
myReverse''' = foldl' (flip (:)) []

-- *** PROBLEM 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).***
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' x = (==) x (myReverse x)

-- *** PROBLEM 7: Flatten a nested list structure.***
data NestedList a = Elem a | List [NestedList a]
-- Above definition of NestedList can be found in Problem 7 decription here https://wiki.haskell.org/99_questions/1_to_10.

myFlatten :: NestedList a -> [a]
myFlatten (Elem a)        = [a]
myFlatten (List [])       = []
myFlatten (List (x : xs)) = myFlatten x ++ myFlatten (List xs)

-- *** PROBLEM 8: Eliminate consecutive duplicates of list elements.***
compress :: Eq a => [a] -> [a]
compress []             = []
compress [x]            = [x]
compress (x1 : x2 : xs) = if x1 == x2
    then      compress (x2 : xs)
    else x1 : compress (x2 : xs)

-- *** PROBLEM 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.***
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x1 : x2 : xs) =
    let
        (r : rs) = pack (x2 : xs)
    in
        if x1 == x2
            then (x1:r) : rs
            else [x1]   : (r : rs)

-- *** PROBLEM 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
--                 Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.***

encode :: Eq a => [a] -> [(Int,a)]
encode = map (\ (s : str) -> (myLength (s : str), s)) . pack

-------------------------------------------------------------------------------- Problems 11 - 20

-- *** PROBLEM 11: Modified run-length encoding.***
data Encoded a = Multiple Int a | Single a
    deriving Show

encodeModified ::Eq a => [a] -> [Encoded a]
encodeModified = map (\ (s : str) -> 
    let
        l = length (s : str)
    in
        if l>1
            then Multiple l s
            else Single s
    ) . pack