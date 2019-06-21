rvrt l = foldr (\x k -> \acc -> k(x:acc)) id l []

r1 = foldr (\x k -> \acc -> k(x:acc)) id [1,2]
r2 = (\x k -> \acc -> k(x:acc)) 1 $ foldr (\x k -> \acc -> k(x:acc)) id [2]
r3 = (\x k -> \acc -> k(x:acc)) 1 $ (\x k -> \acc -> k(x:acc)) 2 $ foldr (\x k -> \acc -> k(x:acc)) id []
r4 = (\x k -> \acc -> k(x:acc)) 1 $ (\x k -> \acc -> k(x:acc)) 2 $ id 
r5 = (\x k -> \acc -> k(x:acc)) 1 $ (\k -> \acc -> k(2:acc)) $ id 
r6 = (\x k -> \acc -> k(x:acc)) 1 $ (\acc -> id (2:acc)) 
r7 = (\x k -> \acc -> k(x:acc)) 1 $ (\acc -> 2:acc) 
r8 = (\k -> \acc -> k(1:acc)) $ (\acc -> 2:acc) 
r9= (\acc -> (\acc' -> 2:acc') $ (1:acc))
r10= \acc -> 2:1:acc