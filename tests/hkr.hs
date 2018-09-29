import Data.List

test :: [[Int]] -> String
test [x] = "YES"
test [x, [1]] = "YES"
test [[x],(y:ys)]
  | x - 1 == y = "YES"
  | otherwise = "NO"
test x = "NO"

main = do
  t <- getLine
  putStrLn . test . reverse . sort . group . map length $ group . sort $ t