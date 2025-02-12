import GHC.Base (VecElem(Int16ElemRep))
-- :cd C:/Users/Harvey/Documents/Year 2/MATHFUN/FUN/


heartMonitor :: Int -> Int -> String
heartMonitor age bpm
    | age > 80 && bpm > 100 = "High heart rate for 81+!"
    | age > 60 && bpm > 130 = "High heart rate for 61-80!"
    | age > 40 && bpm > 140 = "High heart rate for 41-60!"
    | age > 20 && bpm > 155 = "High heart rate for 21-40!"
    | age >= 0 && bpm > 170 = "High heart rate for 0-20!"
    | otherwise = "Normal heart rate"

pizzaCalories :: Int -> String -> Float
pizzaCalories d toppings = (11.5 + toppingCalories) * area
    where 
        area = pi * (fromIntegral d / 2) ^ 2
        toppingCalories
            | toppings == "veggie" = 2.5
            | toppings == "tuna" = 4
            | toppings == "pepperoni" = 6
            | otherwise = 0

absolute :: Int -> Int
absolute num
    | num >= 0 = num
    | num < 0 = -num

sign :: Int -> Int
sign num
    | num > 0 = 1
    | num == 0 = 0
    | num < 0 = -1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | x /= y && x /= z = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = dlA + dlB + dlC
    where
        dlA = sqrt (2 * (x ^ 2))
        dlB = sqrt (2 * (y ^ 2))
        dlC = sqrt (2 * (z ^ 2))

taxiFare :: Int -> Float
taxiFare distance = base + firstRate + secondRate
    where
        base = 2.20
        firstRate = min 10 (fromIntegral distance) * 0.50
        secondRate = max 0 (fromIntegral distance - 10) *  0.30

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = length(filter (> avg) [fromIntegral x,fromIntegral y,fromIntegral z])
    where
        avg = fromIntegral (x + y + z) / 3

{- 
Test Cases
howManyAboveAverage 5 5 5 - output: 0
howManyAboveAverage 2 5 9 - output: 1
howManyAboveAverage 1 5 7 - output: 2
-}

validDate :: Int -> Int -> Bool
validDate day month
    | month < 1 || month > 12 = False
    | day < 1 = False
    | month == 2 = day <= 28
    | month `elem` [4, 6, 9, 11] = day <= 30
    | otherwise = day <= 31

daysInMonth :: Int -> Int -> Int 
daysInMonth month year
    | month < 1 || month > 12 = error "Invalid Month"
    | month == 2 = if isLeapYear year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31
    where
        isLeapYear y = y `mod` 4 == 0

{-
Evaluation:
sumThree 3 5 7:
3 + 5 + 7
= 8 + 7
= 15

sumThree 8 (1 + 3) 2:
8 (4) 2
= 8 + 4 + 2
= 12 + 2
= 14

threeDifferent 1 4 2 (a /= b && a /= c && b /= c)
1 /= 4 && 1 /= 2 && 4 /= 2
= true && true && true
= true
break;

threeDifferent 1 7 7
1 /= 7 && 1 /= 7 && 7 /= 7
= true && true && False
= False
break;

howManyEqual 3 5 2 (
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | x /= y && x /= z = 0
)
| x == y && x == z = 3
3 == 5 && 3 == 2
= false && false
= false

| x == y || x == z || y == z = 2
3 == 5 || 3 == 2 || 5 == 2
= false || false || false
= false

| x /= y && x /= z
= 3 /= 5 && 3 /= 2
= true && true
= true
= 0
break;

howManyEqual 5 2 5
| x == y && x == z = 3
5 == 2 && 5 == 5
= false && true
= false

| x == y || x == z || y == z = 2
5 == 2 || 5 == 5 || 2 == 5
= false || true || false
= true
= 2
break;
-}