newtype Part a = Part {runPart::(a -> Maybe a)}

apply :: Part a -> a -> a
apply f v = case runPart f v of
    (Just v') -> v'
    _ -> error "Match error"

isDefinedAt :: Part a -> a -> Bool
isDefinedAt p x = case runPart p x of
        (Just _) -> True
        _ -> False

orElse :: Part a -> Part a -> Part a
orElse pf1 pf2 = Part $ \x -> case runPart pf1 x of
    Nothing -> runPart pf2 x
    just -> just
    

tFun1 :: Int -> Maybe Int
tFun1 x | x == 1 = Just $ x+1
        | otherwise = Nothing
tFun2 :: Int -> Maybe Int
tFun2 x | x == 2 = Just $ x*2
        | otherwise = Nothing
tFun3 :: Int -> Maybe Int
tFun3 x | x == 3 = Just $ x*10
        | otherwise = Nothing

p1 = Part tFun1
p2 = Part tFun2
p3 = Part tFun3
pComp = p1 `orElse` p2 `orElse` p3