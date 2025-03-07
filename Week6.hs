{- Week6.hs
This module illustraes the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive =  filter (>0)

keepDigits :: String -> String 
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

{- Worked example 1
Always even can be shortened by using the andAll function
-}
andAll :: [Bool] -> Bool
andAll xs = foldr (&&) True xs

alwaysEven :: (Int -> Int) -> [Int] -> Bool
{-
Solution 1:
alwaysEven f xs = length (filter even (map f xs)) == length xs
-}
{-
Solution 2:
alwaysEven f xs = andAll (map (even . f) xs)
-}
{-
Solution 3:
-}
alwaysEven f = andAll . map (even . f)


-- Worked Example 2:
{-
Write a function with the signature below that takes a function and a list. 
It should only apply the function to the positive numbers in the list; 
non-positive numbers should remain unchanged.
-}
updatePositivesOnly :: (Float -> Float) -> [Float] -> [Float]
-- Solution 1:
{-
updatePositivesOnly _ [] = []
updatePositivesOnly f (x : xs)
  | x > 0 = f x : updatePositivesOnly f xs
  | otherwise = x : updatePositivesOnly f xs
-}

-- Solution 2: Using lambda function instead
updatePositivesOnly f = map (\x -> if x > 0 then f x else x)

-- **Programming Exercises**:
-- 1. write a function mult10 that multiplies every element of a list by 10

mult10 :: [Int] -> [Int]
mult10 list = map (*10) list

-- 2. write a function onlyLowerCase that uses the isLower function to remove 
-- any character from a string that is not a lowercase letter.

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- 3. write a one-line definition for the orAll function from last week.
orAll :: [Bool] -> Bool
orAll xs = foldr (||) False xs

-- 4. write a one-line definition for sumSquares (from Worksheet 3)
sumSquares :: [Int] -> Int
sumSquares = foldr (\x acc -> x * x + acc) 0 

-- 5.write a function zeroToTen that takes a list of integers and only 
-- keeps those that are between 0 and 10 (inclusive)
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> x >= 0 && x <= 10)

-- 6. write a function squareRoots with the signature below. 
-- squareRoots should return a list of the square roots of all the non-negative values in the given list.
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

-- 7. write a function countBetween that counts the number of items 
-- in a list that are between the specified lower- and upper-bounds values.
-- (those equal to either bound should be counted)
countBetween :: Float -> Float -> [Float] -> Int
countBetween a b xs = length (filter (\x -> x >= a && x <= b) xs)

-- 8. write a function alwaysPositive that tests whether applying a given function
-- to all the elements of a list results only in positive values.
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- write three definitions of this function similar to the first worked example
{- Solution 1:
alwaysPositive f xs = foldr (&&) True (map (\x -> f x > 0) xs)
-}
{- Solution 2:
alwaysPositive f xs = andAll (\x -> f x > 0) xs)
-}
{- Solution 3: 
alwaysPositive f = andAll . map (\x -> f x > 0)
-}

-- 9. write a function productSquareRoots that returns the product of the square roots 
-- of all non-negative numbers in the given list
{-
productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (*) 1 (squareRoots xs)
-}


-- Complete the following questions **WITHOUT** using map, filter or foldr.
-- 10. write a recursive polymorphic function removeFirst that removes the 
-- first element of a list that has a certain property
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x:xs)
  | f x       = xs 
  | otherwise = x : removeFirst f xs

-- 11. using your removeFirst function and any other function from the Prelude
-- you find useful, write a one-line removeLast function that removes the last
-- element of a list that has a certain property.
removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f xs = reverse (removeFirst f (reverse xs))

{- Using Lambda Functions:
Lambda Expression (anonymous) || Standard Function (named)
------------------------------------------------------------
        \x -> 2 * x           ||        Mult2 x = x * 2
       \x y -> x * y          ||   multiply x y = x * y
   \x -> x < 0 && even x      || evenNegative x = x < 0 && even x
-}

-- 12. using filter and a lambda expression, 
-- write an alternative solution to exercise 5.
-- vvv avoiding naming conflict
zeroToTen2 :: [Int] -> [Int]
zeroToTen2 = filter (\x -> x >= 0 && x <= 10)

{-
13. [harder] we can rewrite pretty much any of your functions 
using foldr (no need for a map or filter). 
Here is a definition of mult10 using foldr:
mult10 :: [Int] -> [Int]
mult10 = foldr (\x xs -> x * 10 : xs) []

another for onlyLowerCase:
onlyLowerCase :: String -> String
onlyLowerCase = foldr (\x xs -> if isLower x then x : xs else xs) []

write similar definitions for the following functions:
-}
-- a.
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f xs = foldr (\x acc -> f x > 0 && acc) True xs

-- b.
-- productSquareRoots :: [Float] -> Float
-- productSquareRoots xs = foldr (\x acc) 

-- c. 
-- reverse
