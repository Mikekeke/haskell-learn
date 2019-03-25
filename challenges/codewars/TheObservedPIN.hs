
variants = [
    ('1', "142")
    ,('2', "2135")
    ,('3', "326")
    ,('4', "4157")
    ,('5', "54682")
    ,('6', "6359")
    ,('7', "748")
    ,('8', "87590")
    ,('9', "986")
    ,('0', "08")
    ]

getPINs :: String -> [String]
getPINs = maybe [] id . fmap sequenceA . traverse (flip lookup variants)

{-
interesting one

variations = [ "08", "124", "2135", "326", "4157", "52468", "6359", "748", "87590", "986"]
getPINs :: String -> [String]
getPINs = sequence . fmap (variations!!) . fmap (\x -> read [x])

best from solutions (doesn't handle ivalid input tho)

subst '0' = "08"
subst '1' = "124"
subst '2' = "1235"
subst '3' = "236"
subst '4' = "1457"
subst '5' = "24568"
subst '6' = "3569"
subst '7' = "478"
subst '8' = "57890"
subst '9' = "689"

getPINs :: String -> [String]
getPINs = mapM subst
-}