{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)
import System.Win32 (xBUTTON1)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

-- ** WORKSHEET START ** --
countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs) 
    | x == ' ' = 1 + countSpaces xs
    | otherwise = countSpaces xs

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y = x : mergeLists xs (y : ys)
    | otherwise = y : mergeLists (x : xs) ys

-- **Programming Excersises** --
-- 1. Write a function headPlusOne that, given a list of integers, 
-- takes the first element, adds one to it and returns the result.
-- The function should return -1 if the input list is empty.
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1

-- 2. Write a polymorphic function called duplicateHead that adds an 
-- extra copy of the first element at the beginning of a given list 
-- (or returns the empty list unchanged):
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

-- 3. Write a polymorphic function rotate that swaps the first two elements of a list 
-- (or leaves the list unchanged if it contains fewer than two elements).
rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x : y : xs) = y : x : xs

-- 4. Write a recursive polymorphic function listLength that returns the length of a list.
-- This is a re-implementation of the Prelude’s length function, so don’t use this in your
-- solution.
listLength :: [a] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

-- 5. Write a recursive function multAll that returns the product of all integers in a list.
multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * (multAll xs)