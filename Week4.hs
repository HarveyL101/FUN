-- **CODE NEEDED FOR WORKSHEET** --
import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)


-- **END OF REQUIRED CODE** -- 


sumEvenNumbersBetween :: Int -> Int -> Int 
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], even i]
-- Line above does the same as lines below... more efficient.
--  sumEvenNumbersBetween x y
--  | x > y = 0
--  | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
--  | otherwise = x + sumEvenNumbersBetween (x + 1) y

sumNumbersBetween :: Int -> Int -> Int
sumNumbersBetween x y = sum [x .. y]
-- Line above is effectively the lines below
-- sumNumbersBetween x y
-- | x > y = 0
-- | otherwise = x + sumNumbersBetween (x + 1) y

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
    where 
        sumMarks = sum [mk | (_, mk) <- stmks]
        numberOfStudents = length stmks

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (_, mark) 
    | mark < 0 || mark > 100 = error "Invalid Mark"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 40 = (name, 40)
    | otherwise = (name, mark)
 
firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

firstSquares :: Int -> [Int]
firstSquares n = [n^2 | n <- [1..n]]

capitalise :: String -> String
capitalise str = [toUpper ch | ch <- str]

-- 7. Using a list comprehension, write a function onlyDigits 
-- that only keeps the numericalcharacters in a string.
onlyDigits :: String -> String
onlyDigits numstr = [n | n <- numstr, isDigit n]

-- 8. Using your capMark function and a list comprehension, 
-- write a function that caps the mark of each student on a given list of StudentMarks.
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark stmk | stmk <- stmks]

-- 9. Using your grade function and a list comprehension, write
-- a function that grades every student on a given list of StudentMarks
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(name, grade (name, mark)) | (name, mark) <- stmks]

-- 10. Write a function duplicate that repeats a string a 
-- given number of times.
duplicate :: String -> Int -> String
duplicate str n = concat [str | _ <- [1..n]]

-- 11. Using a list comprehension, write a function that 
-- lists all the divisors of a number.
divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], mod x i == 0]

-- 12. Using your divisors function, write a function that 
-- checks whether a number is prime.
isPrime :: Int -> Bool
isPrime x
    | length (divisors x) > 2 = True
    | otherwise = False

-- 13. Using list comprehensions, write a polymorphic function 
-- (i.e., a function that works for any type a and b) that makes 
-- a list of pairs into a pair of lists. (using tuple unpacking)
split :: [(a, b)] -> ([a], [b])
split xs = ([a | (a, _) <- xs], [b | (_, b) <- xs])

-- split using fst() and snd() functions
-- split :: [(a, b)] -> ([a], [b])
-- split xs = (map fst xs, map snd xs)