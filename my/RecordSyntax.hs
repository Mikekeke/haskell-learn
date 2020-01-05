
data Person = Person {name :: String, age :: Int} deriving Show

bob = Person "Bob" 23

testPrint person = print person
test1 = testPrint bob {name = "Lol"}
{-
λ: test1
Person {name = "Lol", age = 23}
-}

testAdd p = testPrint p {name = ("Lol " ++ name p)}
{-
λ: testAdd bob
Person {name = "Lol Bob", age = 23}
-}