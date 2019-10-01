import Test.QuickCheck
import Control.Applicative
import Data.Function

--Q3.2
counter x = (\x -> x + 1) $ ((\x -> x + 1) x)

counterAns x = (\x -> x + 1)
                ((\x -> x + 1)
                    ((\x -> x) x))

tstQ32 = quickCheck $  (liftA2 (==) counter counterAns :: Int -> Bool)

-------
mCycle :: [a] -> [a]
mCycle xs = xs ++ mCycle xs

mCycle' :: [a] -> [a]
mCycle' xs = fix (xs ++)

-- hangs
-- tstCycle = quickCheck $ (liftA2 ((==) `on` (take 10)) mCycle mCycle' :: String -> Bool)

--Q8.
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- foldl
reverse2 l = go [] l where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

reverse3 l = go l id [] where
    go [] accF = accF
    go (x:xs) accF = (go xs accF) . (x:) 

reverse3_1 l = go l [] where
    go [] = id
    go (x:xs) = (go xs) . (x:) 

r3 l = foldr (\x b -> b . (x:)) id l [] 

tstRev myRev = quickCheck (liftA2 (==) reverse myRev :: [Int] -> Bool)
tstQ81 = mapM_ tstRev [reverse1, reverse2, reverse3, reverse3_1]

-- robots

data Robot = Robot {name :: String, attack :: Int, hp :: Int} deriving Show
fight :: Robot -> Robot -> Robot
fight (Robot _ a1 _)  (Robot n a2 h) = Robot n a2 (h-a1)

crazyBob = Robot "Bob" 20 90
bigTom = Robot "Tom" 10 150

fRound r1 r2 = fight (fight r1 r2) r1

threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight r1 r2 = 
    (\r1 r2 -> 
        (\r1 r2 -> 
            (\r1 r2 -> 
                (\r1 r2 -> 
                    (\r1 r2 -> 
                        (\r1 r2 -> fight r2 r1)
                    r1 (fight r1 r2))
                (fight r2 r1) r2) 
            r1 (fight r1 r2)) 
        (fight r2 r1) r2) 
     r1 (fight r1 r2) 
    ) r1 r2

test = threeRoundFight crazyBob bigTom

-- robot :: (String, Int, Int) -> ((String, Int, Int) -> a) -> a
-- robot (name,attack,hp) = \message -> message (name,attack,hp)
-- printRobot aRobot = aRobot (\(n,a,h) -> n ++
--                         " attack:" ++ (show a) ++
--                         " hp:"++ (show h))
-- getName _robot = _robot $ \(n,_,_) -> n
-- getAttack _robot = _robot $ \(_,a,_) -> a
-- getHp _robot = _robot $ \(_,_,h) -> h
-- setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
-- setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
-- setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

-- damage :: (((String, Int, Int) -> a) -> a) -> Int -> ( ((String, Int, Int) -> a) -> a)
-- damage aRobot attackDamage = aRobot $ \(n,a,h) -> robot (n,a,h-attackDamage)
-- -- fight :: (((String, Int, Int) -> a) -> a) -> (((String, Int, Int) -> a) -> a) -> (((String, Int, Int) -> a) -> a)
-- fight aRobot defender = damage defender attack
--     where attack = if getHp aRobot > 10 
--                     then getAttack aRobot
--                     else 0

-- crazyBob :: ((String, Int, Int) -> a) -> a
-- crazyBob = robot ("Bob", 20, 90)
-- bigTom :: ((String, Int, Int) -> a) -> a
-- bigTom = robot ("Tom", 10, 150)

-- r1 = fight crazyBob bigTom
-- r2 = \rb ->  fight rb crazyBob
-- r2' = 
--     (
--         \ra rb ->  fight rb ra
--     ) crazyBob bigTom 
    
-- threeRoundFight :: (((t1, t2, t2) -> t2) -> a) -> (((String, Int, Int) -> ((String, Int, Int) -> a) -> a) -> t) -> t
-- threeRoundFight ra rb = 
--     let
--         rb1 = fight ra rb
--         -- ra1 = fight rb1 ra
--     in rb1


-- tt = printRobot $ threeRoundFight crazyBob bigTom
