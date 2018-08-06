import           Data.Char

newtype Parser a = Parser {apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p
-- parse = ((fst . head) .) . apply

anyChar :: Parser Char
anyChar  = Parser f where
    f ""     = []
    f (c:cs) = [(c,cs)]

instance Functor Parser where
    fmap f p = Parser $ \s -> map (\(a,b) -> (f a, b)) (apply p s)

    -- stuff version
    -- fmap f p = Parser fun where
    --         fun s = [(f a, s') | (a, s') <- apply p s]


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
