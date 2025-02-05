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

    