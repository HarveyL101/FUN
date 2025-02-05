-- :cd C:/Users/Harvey/Documents/Year 2/MATHFUN/FUN/


sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = (d * pi) * h

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

timesTen :: Int -> Int
timesTen n = n * 10

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle r = (r * r) * pi

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = h * areaOfCircle r

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

divisibleBy :: Int -> Int -> Bool
divisibleBy x y =  mod x y == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3

absolute :: Int -> Int
absolute x = if x < 0 then -x else x