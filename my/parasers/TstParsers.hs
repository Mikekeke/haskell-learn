module TstParsers where

-- http://dev.stephendiehl.com/fun/002_parsers.html

import           Control.Applicative
import           Control.Monad
import           Data.Char


-- to undersand
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap _ []     = []
myConcatMap f (x:xs) = (f x) ++ (myConcatMap f xs)

myConcatMap2 f = foldr ((++) . f) []
-- to undersand


newtype Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser parser input = case parse parser input of
    [(res, [])] -> res
    [(_, rest)] ->  error $ "Parser did not consume entire stream. Left: " ++ (show rest)
    _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser $ \s -> [(a,s)]

instance Functor Parser where
    fmap f p = Parser $ \s -> [(f a, b) | (a, b) <- parse p s]
