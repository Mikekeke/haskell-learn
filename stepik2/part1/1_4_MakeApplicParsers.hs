import           Data.Char
import Control.Applicative

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

char :: Char -> Parser Char
char c = satisfy (==c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplicatoin ::  Parser Int
multiplicatoin = (*) <$> digit <* char '*' <*> digit

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
