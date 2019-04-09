import           Text.Parsec

getList :: Parsec String u [String]
getList = many1 digit `sepBy1` char ';'
-- parseTest getList "1;234;56" -> ["1","234","56"]
-- parse getList "" "1;234;56" -> Right ["1","234","56"]

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces open close dt = open *> dt <* close 
test = ignoreBraces (string "[[") (string "]]") (many1 letter)
