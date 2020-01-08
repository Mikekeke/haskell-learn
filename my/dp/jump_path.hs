import qualified Data.Map.Strict as M
import           GHC.Arr         as A

-- track = "101110101010111101010101111111111111111010101111111110101"
track = "1011101010101"
-- track = "1110101"
convert '1' = True
convert '0' = False

trackArr t = A.listArray (1, length t) $ map convert t

canJump from to = case (from, to) of
    ('1', '1') -> True
    _          -> False

solve :: String -> [[(Int, Int)]]
solve t = go (zip [1..] t) where
    go :: [(Int, Char)] -> [[(Int, Int)]]
    go (_:[]) =[]
    go (x:y:[]) = if canJump (snd x) (snd y) then [[(fst x, fst y)]] else []
    go (x:y:z:[]) = l1 ++ l2 where
        l1 = if canJump (snd x) (snd y) then [[(fst x, fst y)]] else []
        l2 = if canJump (snd x) (snd z) then [[(fst x, fst z)]] else []
    go (x:y:z:xs) = l1 ++ l2 where
        l1 = if canJump (snd x) (snd y) then ((fst x, fst y):) <$> (go (y:z:xs)) else []
        l2 = if canJump (snd x) (snd z) then ((fst x, fst z):) <$> (go (z:xs)) else []

-- solve2 :: String -> [[(Int, Int)]]
solve2 t = mem where
    n = length t
    fn :: Int -> [[Int]]
    fn ix | ix == n = [[n]]
          | ix + 1 == n = fmap ((arr ! ix):) ( mem ! (ix+1))
          | otherwise = fmap (x:) (r1 ++ r2) where
              x = arr ! ix
              r1 = mem ! (ix+1)
              r2 = mem ! (ix+2)
    -- arr = A.listArray (1,n) t
    arr = A.listArray (1,n) [1..]
    mem = A.listArray (1,n) (fn <$> [1..])
