import Text.Parsec
import Data.Char

data Abc = Abc Char Char Char deriving Show
getChr n s = s !! n
parceAbc= Abc <$> getChr 0 <*>  getChr 1 <*>  getChr 2
tstAbc = parceAbc "abc"

data Gender = M | F | NA deriving (Eq, Show)
data Person = Person {name::String, age::Int, gender::Gender} deriving Show

rarserGender :: Parsec String u Gender
parserMale = M <$ (string "M" <|> string "male")
parserFemale = F <$ (string "F" <|> string "female")
rarserGender =  parserMale <|> parserFemale <|> pure NA

parserName :: Parsec String u String
parserName = many1 (satisfy isAlpha)

parserAge :: Parsec String u Int
parserAge = read <$> many1 digit

parserPerson = Person <$> parserName <*> (space *> parserAge) <*> (optional space *> rarserGender)
validatePerson p | age p > 100 = Left "bad age"
                 | otherwise = Right p

good1 = "Bob 43 M"
good2 = "Bob 43"
bad = "Bob 800 M"
tstParsePerson = parseTest (validatePerson <$> parserPerson) 