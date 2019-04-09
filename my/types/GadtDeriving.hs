{-# LANGUAGE GADTs #-}
-- OK
data Male
data Female
type Days = Int

data Person gender where
    Dead :: Days -> Person gender
    Dying :: {daysLeft :: Days} -> Person gender
    Alive :: { name :: String
              , weight :: Float
              , father :: Person gender } -> Person gender
     deriving Show

-- NOK
{-
read about standalone
* Can't make a derived instance of `Show (Temp unit)':
        Constructor `Cels' has existentials or constraints in its type
        Constructor `Fahr' has existentials or constraints in its type
        Possible fix: use a standalone deriving declaration instead
    * In the data declaration for `Temp'
-}

-- data C
-- data F

-- data Temp unit where
--     Cels :: Int -> Temp C
--     Fahr :: Int -> Temp F
--     deriving Show
