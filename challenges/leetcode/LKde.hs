-- https://leetcode.com/problems/add-two-numbers/
n1 = [9,9]
n2 = [4,3,1]

n3 = [2,4,3]
n4 = [5,6,4]

getNxt :: [Integer] -> (Integer, Integer)
getNxt a = let n = sum a in if n >=  10 then ((n - 10), 1) else (n,0)
getNxt [] = error "Whoops!" -- gues non emty list will be better here

summ' :: Integer -> [Integer] -> [Integer] -> [Integer]
summ' 0 [] [] = []
summ' x [] [] = [x] 
summ' xt (x:xs) [] = let (x',ext) = getNxt [x,xt] in x' : summ' ext xs []
summ' xt (x:xs) (y:ys) = let (x',ext) = getNxt [x,y,xt] in x' : summ' ext xs ys
summ' xt l r = summ' xt r l

summ = summ' 0