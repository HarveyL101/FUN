-- a simple function definition
rectanglePerimeter :: Int -> Int -> Int
rectanglePerimeter wid len = 2 * (wid + len)

-- a function with an if-then-else expression
describeNumber :: Int -> String
describeNumber x = if x < 0 then "neg" else "non-neg"

-- an alternative to the above that uses a guard
describeNumber' :: Int -> String
describeNumber' x
    | x < 0     = "neg"
    | otherwise = "non-neg" 

-- a function with three guards
describeNumber'' :: Int -> String
describeNumber'' x
    | x < 0     = "neg"
    | x == 0    = "zero"
    | otherwise = "positive"

-- a function illustrating good use of guards
describeTriangle :: Float -> Float -> Float -> String
describeTriangle side1 side2 side3
    | side1 == side2 && side1 == side3                   = "equilateral"
    | side1 == side2 || side1 == side3 || side2 == side3 = "isosceles"
    | otherwise                                          = "scalene"

-- a function illustrating use of local definition to improve code reability
rectanglePerimeter' :: Int -> Int -> Int -> Int -> Int
rectanglePerimeter' tl_x tl_y br_x br_y = 2 * (wid + len)
    where
    wid = br_y - tl_y 
    len = br_x - tl_x

-- a function illustrating guards and local definitions
rectangleCategory :: Int -> Int -> String
rectangleCategory wid len
    | area > 100       = "big"
    | area > 50        = "medium"
    | otherwise        = "small"
    where
    area = len * wid

-- a function illustrating local definitions that use another local definition
trianglePerimeter :: Float -> Float -> Float -> Float -> Float -> Float -> Float 
trianglePerimeter x1 y1 x2 y2 x3 y3 = side1 + side2 + side3
    where
    side1 = dist x1 y1 x2 y2
    side2 = dist x2 y2 x3 y3
    side3 = dist x1 y1 x3 y3
    dist x y x' y' = sqrt ((x-x')^2 + (y-y')^2)
