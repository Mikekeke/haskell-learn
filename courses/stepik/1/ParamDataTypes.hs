module ParamDataTypes where

import Data.Char(isDigit, isSpace)
import Data.Function
import Data.List
import Data.List.Split




data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1-y1) + abs (x2-y2)
--or
manhDistance' :: Coord Int -> Coord Int -> Int
manhDistance' (Coord x1 y1) (Coord x2 y2) = ((+) `on` (abs)) (x2 - x1) (y2 - y1)



findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (c:cs) = if isDigit c then Just c else findDigit cs
--or
findDigit' :: [Char] -> Maybe Char
findDigit' = find isDigit

findDigitOrX :: [Char] -> Char
findDigitOrX s = case findDigit s of
    (Just n) -> n
    _ -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList (Just n) = [n]
maybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

--
tsts = "firstName = John\nlastName = Connor\nage = 30"

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parsePerson :: String -> Either Error Person
parsePerson s = parse where
        ns = splitOn ("\n") s
        dataParts = map (filter (not.isSpace) . (flip (!!) 1) . splitOn("=")) ns
        readInt cs = read cs :: Int
        parse
            | length ns /= 3 = Left ParsingError
            | not $ all (any (== '=')) ns = Left ParsingError
            | not $ all (\x -> (length x) > 0) $ dataParts = Left IncompleteDataError
            | not . all isDigit . last $ dataParts = Left $ IncorrectDataError (last dataParts)
            | otherwise = Right (Person (head dataParts) (dataParts !! 1) (readInt $ dataParts !! 2))