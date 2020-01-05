data Prisoner  = Prisoner {prisonerId :: Int, name :: String} deriving Show
data Camera = Camera {cameraId :: Int, prisoners :: [Int]} deriving Show

prisonersA = [
    Prisoner 1 "Bob"
    , Prisoner 2 "Tom"
    , Prisoner 3 "Sid"
    ]

camerasA = [
    Camera 1 [1,3]
    , Camera 2 [2]
    ]
qe = id
all_ = id
select_ :: (a -> b) -> ((a -> b) -> z) -> z
select_ selector = \c -> c selector
from_ :: (a -> b) -> [a] -> ([b] -> z) -> z
from_ selector db = \c -> c (fmap selector db)
where_ :: (a -> Bool) -> [a] -> ([a] -> z) -> z
where_ pred table = \c -> c (filter pred table)

join = error "TODO"

tst1 = (select_ name from_ prisonersA) qe
tst2 = (select_ name from_ prisonersA) (where_ (('B' ==) . head)) qe
tst3 = (select_ all_ from_ prisonersA) (where_ (('B' ==) . head . name)) qe