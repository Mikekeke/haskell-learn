import           Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken c
    |c == "(" = Just LeftBrace
    |c == ")" = Just RightBrace
    |c == "+" = Just Plus
    |c ==  "-" = Just Minus
    |all isDigit c = Just $ Number $ read c
    |otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map asToken . words

tokenize' :: String -> Maybe [Token]
tokenize' inp = foldr f (return []) (words inp) where
     f word mbList  = do
        list <- mbList
        token <- asToken word
        return $ token:list

tokenize'' :: String -> Maybe [Token]
tokenize'' inp = foldr f (return []) (words inp) where
    f word mbList  = mbList >>= \l -> (asToken word) >>= \w -> Just $ w:l
