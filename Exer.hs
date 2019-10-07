import Debug.Trace

f "0" = ""
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
f ('0':d) = f d
f [d,u] = f [d,'0'] ++ "-" ++ f [u]

data Order = Hundred | Thousand | Million | Trillion deriving Eq
data SayPart = SayPart Order String
instance Show Order where
    show Hundred  = "hundred"
    show Thousand = "thousand"
    show Million  = "million"
    show Trillion = "trillion"

orders = [Hundred, Thousand, Million, Trillion]

split3 :: String -> [String]
split3 = fmap (dropWhile ('0' ==)) . (foldr f [[]]) where
    f c (h:t) | length h == 3 = [c] : h : t
              | otherwise = (c : h) : t

sayPart :: String -> String             
sayPart [n] = f [n]
sayPart n@[_,_] = f n
-- sayPart ['0',n2,n3] = sayPart [n2,n3] 
sayPart [n1,n2,n3] = unwords [f (n1:""), show Hundred, sayPart [n2,n3]] 

sayHundreds :: String -> String             
sayHundreds [n] = f [n]
sayHundreds n@[_,_] = f n
-- sayHundreds ['0',n2,n3] = sayHundreds [n2,n3] 
sayHundreds "100" = unwords [f "1", show Hundred]
sayHundreds [n1,n2,n3] = unwords [f (n1:""), show Hundred, "and", sayHundreds [n2,n3]]

-- tailingHundred "100" = sayHundreds "100"
-- tailingHundred l@['0',_,_] = "and " ++ sayHundreds l
tailingHundred l@[_,_,_]   = sayHundreds l
tailingHundred l           = "and " ++ sayHundreds l


say s = 
    let
        splitted = reverse $ split3 s
        go :: [String] -> [Order] -> [String] -> [String]
        go acc _ [] = acc
        -- go acc (_:os) ("000":ws) = go acc os ws
        go _ (Hundred:_) (h:[]) = [sayHundreds h]
        go acc (Hundred:os) (h:more) = go (tailingHundred h : acc) os more
        go acc (o:os) (d:more) = go ((sayPart d ++ " " ++ show o) : acc) os more
    in unwords $ go [] orders (traceShowId splitted)