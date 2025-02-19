-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)
import Data.Bool (bool)
import System.Win32 (xBUTTON1)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 && 

-- A naive re-implementation of the Prelude operator ||
{-
(||) :: Bool -> Bool -> Bool
True || True   = True
False || True  = True
True || False  = True
False || False = False
-}

-- An alternative re-implementation
{-
(||) :: Bool -> Bool -> Bool
False || False   = False
_ || _           = True
-}

-- Another alternative re-implementation
(||) :: Bool -> Bool -> Bool
True || _     =  True
False || a    = a

-- Self made naive implementation of the 'and' operator &&
{-
(&&) :: Bool -> Bool -> Bool
False && False = True
False && True  = False
True && False  = False
True && True   = True
-}

-- Improved reimplementation
(&&) :: Bool -> Bool -> Bool
False && _ = False
True && a  = a





fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _ = False

fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

exOr :: Bool -> Bool -> Bool
exOr a b
    | a /= b = True
    | a == b = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse False _ y = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth m
    | m == 4 || m == 6 || m == 9 || m == 11  = 30
    | otherwise = 31

-- simpler version of validDate
validDate :: Int -> Int -> Bool
validDate day month = day >= 1 && day <= daysInMonth month

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumSquares :: Int -> Int 
sumSquares 0 = 0
sumSquares n = n*n + sumSquares (n - 1)

power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y 
    | x > y = 0
    | otherwise = x + sumFromTo (x + 1) y

gcd :: Int -> Int -> Int
gcd x 0 = x
gcd x y = gcd y (x `mod` y)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
    | s * s <= n && (s + 1) * (s + 1) > n = s
    | otherwise = findRoot n (s - 1)
 