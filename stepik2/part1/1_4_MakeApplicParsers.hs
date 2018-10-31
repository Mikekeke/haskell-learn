import           Control.Applicative
import           Data.Char

-- lection
newtype Parser a = Parser {apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p
-- parse = ((fst . head) .) . apply

anyChar :: Parser Char
anyChar  = Parser f where
    f ""     = []
    f (c:cs) = [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser fn where
    fn "" = []
    fn (c:cs) | p c = [(c, cs)]
              | otherwise = []

lower = satisfy isLower
-- apply lower "Abc" ~> []

lowers = (:) <$> lower <*> lowers <|> pure ""
-- lowers = pure (:) <*> lower <*> lowers <|> pure ""

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplicatoin ::  Parser Int
multiplicatoin = (*) <$> digit <* char '*' <*> digit

many' :: Parser a -> Parser [a]
many' p = (:) <$> p <*> many' p <|> pure []

instance Functor Parser where
    fmap f p = Parser $ \s -> map (\(a,b) -> (f a, b)) (apply p s)

    -- stuff version
    -- fmap f p = Parser fun where
    --         fun s = [(f a, s') | (a, s') <- apply p s]

instance Applicative Parser where
    pure a = Parser $ \s -> [(a,s)]

    pf <*> pa = Parser fun where
        fun s = [(g a, s2) | (g, s1) <- apply pf s, (a, s2) <- apply pa s1]
-- apply ((,) <$> anyChar <*> anyChar) "ABCD" ~> [(('A', 'B'), "CD")]
--            ^^^ здесь комбинация 3х парсеров: (pure (,) <*> anyChar <*> anyChar), pure (,) ничего не делает с воодомштыеф

instance Alternative Parser where
    empty = Parser $ \_ -> []
    lp <|> rp = Parser $ \s ->
        case apply lp s of
            [] -> apply rp s
            rs -> rs


{-
(from where all this comment started: (replicateM 3 digit) will parse 3 digit - how?)
e.g 
fn :: String -> [(Char, String)]
fn = \s -> [(head s, tail s)]
replicate 2 (Parser fn)
~> [Parser (\s -> [(head s, tail s)]), Parser (\s -> [(head s, tail s)])]
(sequence == sequenceA == traverse id == foldr (\x ys -> (:) <$> (id x) <*> ys) (pure [])
traverse id [Parser (\s -> [(head s, tail s)]), Parser (\s -> [(head s, tail s)])]   
 ~> foldr (\x ys -> (:) <$> (id x) <*> ys) (pure []) [Parser (\s -> [(head s, tail s)], Parser (\s -> [(head s, tail s)]]
 ~> (:) <$> id (Parser (\s -> (head s, tail s))) <*> (foldr (\x ys -> (:) <$> (id x) <*> ys) (pure []) [Parser (\s -> [(head s, tail s)]])
 ~> (:) <$> Parser (\s -> [a1, b1)]) <*> ((:) <$> Parser (\s -> [(a2, b2)]) <*> Parser (\s-> [([],s)]))
 ~> (:) <$> Parser (\s -> [a1, b1)]) <*> (Parser (\s -> map (\(a,b) -> ((:) a, b)) (apply (Parser (\s -> [(a2, b2)])) s)) <*> Parser (\s-> [([],s)]))
 ~> (:) <$> Parser (\s -> [a1, b1)]) <*> (Parser (\s -> map (\(a,b) -> ((:) a, b)) (\s -> [(a2, b2)]) $ s)) <*> Parser (\s-> [([],s)]))
-}

-- якобы разбирает неоднозначно
crazyChar :: Parser Char
crazyChar  = Parser f where
    f ""     = []
    f (c:cs) = [(pred c,cs), (c,cs),(succ c,cs)] 
tst1 =  apply ((,) <$> anyChar <*> crazyChar) "059"
-- ~> [(('0','4'),"9"),(('0','5'),"9"),(('0','6'),"9")]
data Ab = Ab Char Char deriving Show
tst2 = apply (Ab <$> crazyChar <*> anyChar ) "ab"
-- ~> [(Ab '`' 'b',""),(Ab 'a' 'b',""),(Ab 'b' 'b',"")]
tst3 = apply (Ab <$> anyChar <*> crazyChar ) "ab"
-- ~> [(Ab 'a' 'a',""),(Ab 'a' 'b',""),(Ab 'a' 'c',"")]


-- *** tasks ***
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }
instance Functor Prs where
    fmap f p = Prs $ \s -> (\(a, s') -> (f a, s')) <$> runPrs p s

anyChr :: Prs Char
anyChr = Prs f where
    f ""     = Nothing
    f (c:cs) = Just (c,cs)

-- some advanced solution from solutions
-- import Data.List (uncons)
-- import Control.Arrow (first)

-- instance Functor Prs where
--   fmap f (Prs p) = Prs $ (fmap . fmap . first) f p

-- anyChr :: Prs Char
-- anyChr = Prs uncons

instance Applicative Prs  where
    pure a = Prs $ \s -> Just (a, s)
    pf <*> pa = Prs $ \s -> do
        (f, s1) <- runPrs pf s
        (a, s2) <- runPrs pa s1
        return (f a, s2)

-- my "from scratch"
instance Alternative Prs where
    empty = Prs $ \_ -> Nothing
--  empty = Prs $ const Nothing

    lp <|> rp = Prs fun where
        fun s = case runPrs lp s of
            Nothing -> runPrs rp s
            result  -> result
-- more from solutions
--  l <|> r = Prs f where f s = runPrs l s <|> runPrs r s
--    coz these Maybes have Al. ^^^^^^^^^^     ^^^^^^^^^^
-- !!! используется Applicative'ность стрелки и Alternative'ность Maybe
-- lp <|> rp = Prs $ (<|>) <$> (runPrs pa) <*> (runPrs pb)

satisfy' :: (Char -> Bool) -> Prs Char
satisfy' p = Prs fn where
    fn "" = Nothing
    fn (c:cs) | p c = Just (c, cs)
              | otherwise = Nothing
char' :: Char -> Prs Char
char' c = satisfy' (==c)
digit' :: Prs Int
digit' = digitToInt <$> satisfy' isDigit

many1' :: Prs a -> Prs [a]
many1' p =  (:) <$> p <*> (many1' p <|> pure [])
-- from solutions
many'' :: Prs a -> Prs [a]
many'' p = (:) <$> p <*> many'' p <|> pure []
many1'' :: Prs a -> Prs [a]
many1'' p =  (:) <$> p <*> many'' p
--many1'' = liftM2 (<*>) ((:) <$>) many''


nat :: Prs Int
nat = read <$> (many1' $ satisfy' isDigit)
-- from soolutions better decomposition (but digit has type Prs Char)
-- digit :: Prs Char
-- digit = satisfy isDigit
-- digits :: Prs String
-- digits = many1 digit

-- nat :: Prs Int
-- nat = fmap read digits

mult :: Prs Int
mult = (*) <$> nat <* char' '*' <*> nat


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
    f "" = Left "unexpected end of input"
    f (c:cs) | p c = Right (c,cs)
             | otherwise = Left $ "unexpected " ++ [c]

instance Functor PrsE where
  fmap f p = PrsE $ \s -> fn <$> runPrsE p s where
    fn (a,s') = (f a, s')

instance Applicative PrsE where
  pure a = PrsE $ \s -> Right (a,s)
  pf <*> pa = PrsE $ \s -> do
    (f, s1) <- runPrsE pf s
    (a, s2) <- runPrsE pa s1
    return (f a, s2)

-- stuff's variant
-- instance Functor PrsE where
--     fmap f = PrsE . (fmap . fmap $ \(a, s) -> (f a, s)) . runPrsE

-- (!) do not even need Monad context to work unlike my variant
--   instance Applicative PrsE where
--     pure x = PrsE $ \s -> Right (x, s)
--     pf <*> px = PrsE $ \s -> case runPrsE pf s of
--                                Left e        -> Left e
--                                Right (f, s') -> runPrsE (f <$> px) s'
--                                                            ^^^ coz px already have Functor instance

-- fmap f1 f2 = \x -> f1 (f2 x) ~> fmap f1 f2 = f1 . f2 ~> fmap = (.)
-- fmap . fmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- PrsE . (fmap . fmap $ \(a, s) -> (f a, s)) . runPrsE
-- PrsE . (fmap . fmap $ \(a, s) -> (f a, s)) (String -> Either String (a, String))
-- получается, 1й fmap - протащит анонимную функцию в стрелку, а 2й - в Either ?
-- keep in mind
-- λ: :t fmap . fmap (+10)
-- fmap . fmap (+10) :: (Num b, Functor f) => (a -> b) -> f a -> f b - ($) (infixr 0 $) в "fmap (+10)" связывает сильнее, чем (.) (infixr 9 .)
-- λ: :t fmap . fmap $ (+10)
-- fmap . fmap $ (+10) :: (Num b, Functor f2, Functor f1) => f1 (f2 b) -> f1 (f2 b) - то, что в решени иот ведущих курса

--fmap . fmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f1 :: (Functor f1, Functor f2, Num b) => f1 (f2 b) -> f1 (f2 b)
f1 = fmap . fmap $ (+10)
f2 :: a -> Maybe a
f2 = Just
test :: Num t => t -> Maybe t
test x = f1 f2 $ x
-- λ: test 12
-- Just 22
