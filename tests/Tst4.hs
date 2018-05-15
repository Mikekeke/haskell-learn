
data Figure = One | Two deriving Show
data Square = Empty | Occ Figure deriving Show

figs = [
     ('a', One)
    ,('b', Two)
    ]

readFig :: Char -> Either String Square

readFig '.' = return Empty
readFig c = Occ <$> maybeToEither errMsg (lookup c figs) where
    errMsg = "Invalid char: " ++ show c
    maybeToEither _ (Just v)  = return v
    maybeToEither err Nothing = Left err

goodBoard = "ab..a"
badBoard = ".a34"
