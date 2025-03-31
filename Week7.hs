import Text.Printf (printf)
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)  
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type 
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String | 
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- WORKED EXAMPLES --

-- Your task is to write a program that stores and processes the details of a collection
-- of cars. The data we store for each car is the name (make & model), details of the engine
-- (petrol, diesel, or electric and its horsepower), and price.
-- Start by defining an appropriate data structure for the scenario. Then write the following
-- functions defined over your type:
-- 1. totalPrice that returns the total price of a list of cars;
-- 2. filterByMake that filters a list of cars by a manufacturer;
-- 3. updatePriceAt that updates the price of a car in the list at a specified index.
-- 4. formatCar that takes an index and returns a formatted string representation of that
-- car, for example: "3- Vauxhall Corsa costs 8000.00 pounds"

type Make = String
type Model = String
type HorsePower = Int
type Price = Float

data EngineType = Petrol | Diesel | Electric
  deriving (Show, Eq)

data Engine = Engine EngineType HorsePower
  deriving (Show)

data CarName = CarName Make Model
  deriving (Show)

data Car = Car CarName Engine Price
  deriving (Show)

testCars :: [Car]
testCars = [
  Car (CarName "Ford" "Fiesta") (Engine Petrol 55) 10000.0,
  Car (CarName "Ford" "Focus") (Engine Diesel 85) 15000.0,
  Car (CarName "Vauxhall" "Corsa") (Engine Petrol 55) 8000.0,
  Car (CarName "Vauxhall" "Astra") (Engine Diesel 81) 12000.0,
  Car (CarName "Vauxhall" "Astra") (Engine Diesel 96) 14000.0,
  Car (CarName "VolksWagen" "Golf") (Engine Electric 81) 20000.0
  ]

getMake :: Car -> Make
getMake (Car (CarName make _) _ _) = make

getModel :: Car -> Model
getModel (Car (CarName _ model) _ _) = model

getPrice :: Car -> Price
getPrice (Car _ _ price) = price

-- 1. Answer
totalPrice :: [Car] -> Float
totalPrice [] = 0
totalPrice (Car _ _ price : cs) = price + totalPrice cs

-- 2. Answer
filterByMake :: String -> [Car] -> [Car] 
-- filterByMake manufacturer cs = [c | c <- cs, getMake c == manufacturer]
-- More elegant answer 
filterByMake manufacturer = filter (\c -> getMake c == manufacturer)

-- 3. Answer 
updatePriceAt :: Int -> Float -> [Car] -> [Car]
updatePriceAt _ _ [] = []
updatePriceAt 0 amount (c : cs) = updatePrice amount c : cs
updatePriceAt index amount (c : cs) = c : updatePriceAt (index - 1) amount cs

updatePrice :: Float -> Car -> Car
updatePrice newPrice (Car name engine _) = Car name engine newPrice 
{-
 As an alternative, we can use the list index operator (!!) to access an element in a list.
 Next you can stick the pieces of the list back together using the functions drop and take:

 updatePriceAt index price cars = take index cars ++ [newCar] ++ drop (index + 1) cars 
  where 
    newCar = updatePrice price (cars !! index)
-}

-- 4. Answer 
formatCar :: [Car] -> Int -> String
formatCar [] _ = ""
formatCar cars i = printf "%d- %s %s costs %.2f pounds" (i + 1) (getMake c) (getModel c) (getPrice c)
  where 
    c = cars !! i


{-
  PROGRAMMING EXERCISES: Enumerated Types
  1. Define an algebraic type Month that represents the twelve months of the year and
  Season that represents the four seasons. See the test cases for exercise 2.
-}


{-
  2. Define a function season that maps months onto (meteorological) seasons. See the
  test cases showing the outputs for February and March. (All seasons are all three
  months long).
-}
