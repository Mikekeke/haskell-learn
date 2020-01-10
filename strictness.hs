{-# LANGUAGE BangPatterns #-}

add :: Int -> Int -> Int
add !x !y = x + y

add2 !x !y = x
tst = add 1 undefined

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) undefined -- (1 + 3)

  putStrLn $ "Five: " ++ show five