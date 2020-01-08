import GHC.Arr

n = 10 
ls = [9, 7, 5, 5, 2, 9, 9, 9, 2, (-1)]

count :: Int -> [Int] -> Array Int Int
count len parents = arr where
    go (-1) = 1
    go n = succ (arr ! n)
    arr = listArray (0, len-1) (map go parents)

main :: IO ()
main = print (count n ls)