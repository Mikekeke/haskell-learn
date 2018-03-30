mvZeroes :: [Int]->[Int]
mvZeroes xs = l ++ r where
    grp xs' = foldr f ([],[]) xs' where
        f x (zs, nzs) = if x == 0 then (x:zs, nzs) else (zs, x:nzs)
    (l,r) = grp xs