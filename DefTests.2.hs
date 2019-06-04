import qualified Data.Map.Strict as M
import Data.List

inp = 
    [

        [
            ("id", "11")
            , ("p_id", "1")
            , ("slug", "l1")
        ]
        ,[
            ("id", "1")
            , ("p_id", "-")
            , ("slug", "root")
        ]
        ,[
            ("id", "11")
            , ("p_id", "11")
            , ("slug", "l2")
        ]
        ,[
            ("id", "12")
            , ("p_id", "11")
            , ("slug", "l2.2")
        ]
        ,[
            ("id", "r")
            , ("p_id", "-")
            , ("slug", "Rroot")
        ]
        ,[
            ("id", "r1")
            , ("p_id", "r")
            , ("slug", "lvl2Rroot")
        ]
    ]

getId :: M.Map String String -> String
getId _m = _m M.! "id"
getPId :: M.Map String String -> String
getPId _m = _m M.! "p_id"
getSlug :: M.Map String String -> String
getSlug _m = _m M.! "slug"

mp = M.fromList <$> inp

mpPrnt :: [(String, M.Map String String)]
mpPrnt = fmap (\m -> (getPId m, m)) mp

roots = filter ((== "-") . fst) mpPrnt
addUrl :: String -> M.Map String String -> M.Map String String
addUrl url = M.insert "url" url

go app sl [] = []
go app sl ml = do
    let (curr, decendants) = partition ((== sl) . fst) ml
    (_, currLevel) <- curr
    let curl = (app ++ ) . (++ (getSlug currLevel)) $ if null app then "" else "-"
        newR = addUrl curl currLevel
    newR : go curl (getId currLevel) decendants
        