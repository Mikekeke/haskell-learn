
--BOARD GAME
-- https://github.com/dstarcev/stepic-haskell/blob/master/src/Module5/Task11.hs
-- testing impl
data Board = Board Int deriving Show

nextPositions :: Board -> [Board]
nextPositions (Board i) = [Board (i * 10 + 1), Board (i * 10 + 2)]
-- trsting impl - END

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
    | n < 0 = []
    | n == 0 = filter pred [b]
    | otherwise = do
            nextB <- nextPositions b
            nextPositionsN nextB (n - 1) pred


-- Pythagorean Triples
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
    a <- [1..x]
    b <- [1..x]
    c <- [1..x]
    True <- return (a^2 + b^2 == c ^2 && c<=x && a < b)
    return (a,b,c)

pythagoreanTriple' :: Int -> [(Int, Int, Int)]
pythagoreanTriple' x = do 
    a <- [1..x]
    b <- [1..x]
    c <- [1..x]
    if a^2 + b^2 == c ^2 && c<=x && a < b then [0] else []
    return (a,b,c)

-- will double output coz [0,1] will force computation to split (look lection for more info)
pythagoreanTripleBroken :: Int -> [(Int, Int, Int)]
pythagoreanTripleBroken x = do 
    a <- [1..x]
    b <- [1..x]
    c <- [1..x]
    if a^2 + b^2 == c ^2 && c<=x && a < b then [0,1] else []
    return (a,b,c)
