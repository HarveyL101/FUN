{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

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
multAll (x : xs) = x * multAll xs

-- 6. Use the && operator to write a recursive function andAll which returns the 
-- conjunction (and) of all the elements of a list. This is a reimplementation of 
-- the Prelude’s and function so do not use this function in your solution.
{-
andAll :: [Bool] -> Bool
andAll [] = True
andAll [x] = x
andAll (x : xs) = x && andAll xs
-}

-- **Better solution**
andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

-- 7. Write a recursive orAll function that returns the disjunction of all elements
-- in the given list (uses the || operator). Hint: Start by thinking about how the 
-- following expression should be evaluated: orAll[]

orAll :: [Bool] -> Bool
orAll [] = False
orAll[x] = x
orAll (x:xs) = x || orAll xs

-- 8. Write a function countIntegers that counts how many times a given integer
-- appears in a list of integers. This function will be similar in structure to worked example 1.
countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers x (y : ys)
    | x == y    = 1 + countIntegers x ys
    | otherwise = countIntegers x ys

-- 9. Write a recursive function removeAll that removes all instances of 
-- a given number from a list of integers.
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll x (y:ys)
    | x == y    = removeAll x ys
    | otherwise = y : removeAll x ys

-- 10. Using removeAll, write a removeAllButFirst function.
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst x (y:ys)
    | x == y    = y : removeAll x ys
    | otherwise = y : removeAllButFirst x ys

-- 11. Recall the StudentMark type synonym from last week. 
-- Write a function that gives a list of the marks for a particular student.
-- Added for unit testing purposes
type StudentMark = (String, Int)
testData :: [StudentMark]
testData = [
    ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
    ]

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((student, mark):xs)
    | name == student = mark : listMarks name xs
    | otherwise       = listMarks name xs

-- 12. Write a recursive function sorted which decides if the elements of a 
-- list are sorted (the output needs to be of type Bool).
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:ys)
    | x < y = sorted ys
    | otherwise = False

-- 13. Write a recursive function prefix which decides if the first list 
-- of integers is a prefix of the second list.
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x : xs) (y : ys)
    | x == y    = prefix xs ys
    | otherwise = False

-- 14. [Harder] Using your prefix function, write a recursive function 
-- subSequence that decides if the first list is contained in the second.
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xs ys
    | prefix xs ys = True
    | otherwise    = subSequence xs (tail ys)