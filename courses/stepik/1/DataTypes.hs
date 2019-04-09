module DataTypes where

data Color = Red | Green | Blue
--
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"


stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue

-- or
-- data Color = Red | Green | Blue deriving (Read)
--
-- stringToColor :: String -> Color
-- stringToColor = read

--
charToInt :: Char -> Int
charToInt c =
    let
        go c' n
            | fromEnum c == n = n
            | otherwise = go c' (n+1)
    in
        go c 0

--
data LogLevel = Error | Warning | Info

-- my cmp
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Warning Info  = GT
cmp Error Info    = GT
cmp Info Warning  = LT
cmp Warning Error = LT
cmp Info Error    = LT
cmp _ _           = EQ

-- other cmp
-- instance Enum LogLevel where
--     fromEnum Error    = 3
--     fromEnum Warning  = 2
--     fromEnum Info     = 1
--
-- cmp' :: LogLevel -> LogLevel -> Ordering
-- cmp' a b | fromEnum a > fromEnum b = GT
--         | fromEnum a < fromEnum b = LT
--         | otherwise               = EQ

-- another cmp
cmp'' :: LogLevel -> LogLevel -> Ordering
cmp'' x y = compare (ord'' x)  (ord'' y) where
   ord'' Error   = 3
   ord'' Warning = 2
   ord'' Info    = 1

--
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

--haha, learn math
distance' (Point x1 y1) (Point x2 y2) = distanceToOrigin (Point (x2-x1) (y2-y1))

-- Shape
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Rectangle a b) = a*b
area (Circle r)      = pi * r^2
--or
area' :: Shape -> Double
area' s = case s of
            (Circle r)      -> pi*r^2
            (Rectangle x y) -> x*y

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _               = False

--pattern guard
isSquare' :: Shape -> Bool
isSquare' rect
    | (Rectangle a b) <- rect = a == b
    | _ <- rect = False


-- works on exercise page only coz "Fail" not defined here
-- data Result' = Success' | Fail' Int
--
-- instance Show Result' where
--     show Success' = "Success"
--     show (Fail' code) =  "Fail: " ++ show code
--
-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' d =
--     case doSomeWork d of
--         (Success, _) -> Success'
--         (Fail, n) -> Fail' n


--person
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p@Person{firstName = n} = p {firstName = if length n > 2 then (head n): "." else n}
