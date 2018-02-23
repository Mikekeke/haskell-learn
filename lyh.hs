module Test2 where

exp1 = "10 4 3 + 2 * -"
rpn :: String -> Double
rpn = head . foldl foldFun [] . words
    where
        foldFun l@(x1:x2:xs) v
            | v == "+" = (x2+x1):xs
            | v == "-" = (x2-x1):xs
            | v == "*" = (x2*x1):xs
            | v == "/" = (x2/x1):xs
        foldFun l v = (read v):l


-- path
inp = [50,10,30,5,90,20,40,2,25,10,8,0]
partit :: [a] -> [((a,a), a)]
partit [] = []
partit (x1:x2:x3:xs) = ((x1, x2), x3) : partit xs

aOrb a b = if a < b then 'A' else 'B'
shouldCross cp
            | cp == 'A' = if c1+b2 < a2 then ['C'] else []
            | cp == 'B' = if c1+a2 < b2 then ['C'] else []

consPath :: (Ord a, Num a) => [((a,a), a)] -> [Char]
consPath (x:xs) =
    let
        step1 = x
        step2 = head xs
        a1 = fst $ fst step1
        b1 = fst $ snd step1
        c = snd step1
        a2 = fst $ fst step2
        b2 = fst $ snd step2
        cp = aOrb a1 b1
        cross = shouldCross cp
    in
        (cp : cross) ++ consPath xs

consPath (x:[]) = (aOrb (fst $ fst x) (fst $ snd x)) : []



foldingFun :: (Ord a, Num a) => [Char] -> (((a,a), a),((a,a), a)) -> [Char]
foldingFun path (((a1,b1), c1),((a2,b2), c2)) =
    let
        aOrb = if a1 < b1 then 'A' else 'B'
        shouldCross cp
                    | cp == 'A' = if c1+b2 < a2 then ['C'] else []
                    | cp == 'B' = if c1+a2 < b2 then ['C'] else []
    in
        (shouldCross aOrb ) ++ (aOrb : path)

findP :: (Ord a, Num a) => [(((a,a), a),((a,a), a))] -> [Char]
findP = foldl foldingFun []

parted = partit inp
partedPrep = zip parted (tail parted)
solve = reverse $ findP partedPrep

