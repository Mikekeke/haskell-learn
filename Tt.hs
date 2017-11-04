paths :: Int -> Int -> Int
paths m n = go n 0
    where
        go h acc
            |h == 0 = acc
            |h == n - 1  = go (h - 1) (acc + m -1)
            |h == n  = go (h - 1) (acc + 1)
            |otherwise = go (h - 1) (acc + m)



sf :: String -> String
sf "shit" = "fuck"
sf rest = rest


bold :: String -> String
bold s = "<b>" ++ s ++ "</b>"
ital s = "<i>" ++ s ++ "</i>"
form = ital . bold

