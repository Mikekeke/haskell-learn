
newtype Parser a = Parser {apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p
-- parse = ((fst . head) .) . apply