moreEvens l = (length . filter even . take 3 $ l) > 1

findOutlier :: [Int] -> Int 
findOutlier xs = head . filter p $ xs where
    p = case moreEvens xs of
        True -> not . even
        False -> even

-- from solutions
-- findOutlier :: [Int] -> Int
-- findOutlier = go . partition even
--   where
--     go (x, y) =
--       if null (drop 1 x)
--         then head x
--         else head y