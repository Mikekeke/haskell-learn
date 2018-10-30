import Text.Parsec
import Data.Char
import Data.Monoid
import Control.Applicative (liftA2)

data Abc = Abc Char Char Char deriving Show
getChr n s = s !! n
parceAbc= Abc <$> getChr 0 <*>  getChr 1 <*>  getChr 2
tstAbc = parceAbc "abc"

goodInp1 = "Bob 43 M"
goodInp2 = "Bob 43"
badInp1 = "Bob 800 M"
badInp2 = "bob 800 M"
printTestsFor f = mapM_ (putStrLn . show . f) testInputs

testInputs = [goodInp1,goodInp2,badInp1,badInp2]

data Gender = M | F | NA deriving (Eq, Show)
data Person = Person {name::String, age::Int, gender::Gender} deriving Show

parserGender :: Parsec String u Gender
parserGender =  parserMale <|> parserFemale <|> pure NA
parserMale = M <$ (string "M" <|> string "male")
parserFemale = F <$ (string "F" <|> string "female")

parserName :: Parsec String u String
parserName = many1 (satisfy isAlpha)

parserAge :: Parsec String u Int
parserAge = read <$> many1 digit

parserPerson = Person <$> parserName <*> (space *> parserAge) <*> (optional space *> parserGender)
validatePerson p | age p > 100 = Left "bad age"
                 | otherwise = Right p

tstParsePerson = parseTest (validatePerson <$> parserPerson)

data Validated e a = Valid a | Invalid e deriving Show
instance Functor (Validated e) where
    fmap f (Valid a)   = Valid (f a)
    fmap _ (Invalid e) = Invalid e

instance Monoid e => Applicative (Validated e) where
    pure = Valid
    Valid f   <*> Valid a = Valid (f a) 
    Invalid l <*> Invalid r = Invalid (l <> r) 
    Invalid l <*> _ = Invalid l 
    _         <*> Invalid r = Invalid r


-- double Applicative
-- https://stackoverflow.com/questions/12587195/examples-of-haskell-applicative-transformers

{- 
It will parse all input with Validated, so it's easier to just parse Person straight and validate ADT
But if use Maybe isnted of Validated, then it not gona parse further (tested wint bad name and "validateAge = undefined")
and will return Nothing (or Right Nothing with runParser).
!!! But maybe better validate and stop it on parser level with domain error - need to figure out how if possible 
-}

(<<$>>) :: (Functor f2, Applicative f1) => f1 (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = liftA2 (<$>)

(<<*>>) :: (Applicative f, Applicative f1) => f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)
(<<*>>) = liftA2 (<*>)

-- sample
p1 =  Right (Valid "Bob")
p2 =  Right (Valid 22)
p3 =  Right (Valid M)
pr :: Monoid e =>  Either a (Validated e (String -> Int -> Gender -> Person))
pr = Right . Valid $ Person
tst :: Monoid e => Either a (Validated e Person)
tst = pr <<*>> p1 <<*>> p2 <<*>> p3
-- ~> Right (Valid (Person {name = "Bob", age = 22, gender = M}))
-- sample - END
validateName :: String -> Validated [String] String
validateName n = case isUpper (head n) of
    True -> Valid n
    _ -> Invalid ["Name must be uppercase"]
    
validateAge ::Int -> Validated [String] Int
validateAge x = case x <= 0 || x > 120 of
    True -> Invalid ["Invalid age: " ++ (show x)]
    False -> Valid x

validatedNameP = validateName <$> parserName
validatedAgeP = validateAge <$> parserAge
validatedGenP = pure <$> parserGender



parserPersonV :: Parsec String u (Validated [String] Person)
-- parserPersonV = pure (Valid Person) <<*>> validatedNameP... - works too
parserPersonV = pure Person 
    <<$>> validatedNameP 
    <<*>> (space *> validatedAgeP) 
    <<*>> (optional space *> validatedGenP)

tstParsePersV :: String -> Either ParseError (Validated [String] Person)
tstParsePersV s = runParser parserPersonV () "" s

runTestsV = printTestsFor tstParsePersV

-- double Applicative - END
