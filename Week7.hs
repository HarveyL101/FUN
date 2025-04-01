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
data Shape = Circle Float | Rectangle Float Float
  deriving (Show)

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
testSearchTree =  Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

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
data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Eq, Ord, Show)
data Season = Spring | Summer | Autumn | Winter
  deriving (Show)
{-
  2. Define a function season that maps months onto (meteorological) seasons. See the
  test cases showing the outputs for February and March. (All seasons are all three
  months long).
-}
season :: Month -> Season
season m 
  | m `elem` [December, January, February]  = Winter
  | m `elem` [March, April, May]            = Spring
  | m `elem` [June, July, August]           = Summer
  | m `elem` [September, October, November] = Autumn

{-
  3. Define a function numberOfdays that takes a month and a year and returns the number
  of days in that month. Assume all years divisible by four are leap years.
-}
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0) 

numberOfDays :: Month -> Int -> Int
numberOfDays February year
  | isLeapYear year = 29
  | otherwise       = 28
numberOfDays month year
  | month `elem` [April, June, September, November] = 30
  | otherwise                                       = 31

{-
  Points And Shapes:

  4. Define an algebraic type Point which should represent the coordinates of points in
  two-dimensional space.
-}
data Point = Point Int Int
  deriving (Show)

{-
  5. Using Shape and Point, define a data type called PositionedShape which combines a
  shape and with its centre point
-}
data PositionedShape = PositionedShape Shape Point
  deriving (Show)

{-
  6. Define a function move with the signature below that moves a given PositionedShape
  by the given x and y distances.
  ```
  move :: PositionedShape -> Float -> Float -> PositionedShape
  ```
  Note that you will need to add deriving (Show) to the data types from exercises 4
  and 5.
-}
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point px py)) x y = PositionedShape shape (Point (px + round x) (py + round y))

{-
  Functions for the binary tree type

  7. Week7.hs defines a recursive type Tree for a binary tree and a testTree value of this
  type. Define a function numberOfNodes that returns the number of nodes in a given
  tree.
-}
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

{-
  8. Define a function with the signature below that determines if a value exists in a tree.
  ```
  isMember :: Int -> Tree -> Bool
  ```
-}
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember x (Node root left right)
  | root == x = True
  | otherwise = isMember x left || isMember x right

{-
  9. Define a function leaves with the signature below that returns the list of all the leaves
  of the tree. Leaves are nodes that have Null as both subtrees (see the test case).
  ```
  leaves :: Tree -> [Int]
  ```
-}
leaves :: Tree -> [Int]
leaves Null = []
leaves (Node root Null Null) = [root]
leaves (Node _ left right) = leaves left ++ leaves right

{-
  10. Define a function inOrder that lists the elements of a tree according to the in-order
  traversal.

  If the tree is a valid binary search tree, this function will give a list of the tree’s
  elements in ascending numerical order. Try it on the testBinaryTree below.
-}
testBinaryTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node root left right) = inOrder left ++ [root] ++ inOrder right

{-
  11. Define a function insert which inserts a new value into a tree. The function should
  assume that the tree is a binary search tree and should preserve this property.

  insert :: Int -> Tree -> Tree
  
  Test your function by inserting 2 to testBinaryTree defined in exercise 10. It should
  produce the following tree:
  
  Node 5 (Node 1 Null (Node 2 Null Null)) (Node 8 (Node 7 Null Null) Null)
  
  If you struggle with the concept, try this visualisation tool in your browser and insert
  the following values one after another: 5, 1, 7, 8, 2.
  https://www.cs.usfca.edu/~galles/visualization/BST.html
-}

insert :: Int -> Tree -> Tree
insert x Null = Node x Null Null
insert x (Node root left right)
  | x < root  = Node root (insert x left) right
  | x > root  = Node root left (insert x right)
  | otherwise = Node root left right

{-
  12. Define a function listToSearchTree which creates a binary search tree from a list of
  integers. You need to insert elements in the order that they appear in the list.

  Finally, using listToSearchTree and inOrder, write another function binaryTreeSort
  that sorts a lit of integers.
-}
listToSearchTree :: [Int] -> Tree
listToSearchTree arr = foldl (\tree x -> insert x tree) Null arr
