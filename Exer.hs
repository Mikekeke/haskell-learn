f "0" = "zero"
f "00" = ""
f "1" = "one"
f "2" = "two"
f "3" = "three"
f "4" = "four"
f "5" = "five"
f "6" = "six"
f "7" = "seven"
f "8" = "eight"
f "9" = "nine"
f "10" = "ten"
f "11" = "eleven"
f "12" = "twelwe"
f "22" = "twenty-two"
f "20" = "twenty"
f "30" = "thirty"
f "40" = "forty"
f "50" = "fifty"
f "60" = "sixty"
f "70" = "seventy"
f "80" = "eighty"
f "90" = "ninghty"
f [d,u] = f (d:"0") ++ " " ++ f (u:[])

type Order = String
orders = ["hundred", "thousand", "million", "trillion"]
g :: String -> String -> String
g _ "000" = ""
g "hundred" [o,d,u] = (f (o:[])) ++ " " ++ "hundred" ++ " " ++ f [d,u]
g order [o,d,u] = (g "hundred" [o,d,u]) ++ " " ++ order
g order [d,u] =  f [d,u] ++ " " ++ order
g order [u] = f [u]  ++ " " ++ order


split3 :: String -> [String]
split3 = foldr f [[]] where
    f c (h:t) | length h == 3 = [c] : h : t
              | otherwise = (c : h) : t

say s = 
    let
        splitted = reverse $ split3 s
        k os ns  = g os ns
    in unwords $ reverse $ zipWith k orders splitted