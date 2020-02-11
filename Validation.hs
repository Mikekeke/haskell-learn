{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Bool
import Control.Applicative

data Validation = Raw | Checked

data Person (v :: Validation) = Person {ids :: [Int], name :: String} deriving Show

type Result = ([String], [Person Checked])
type ErrorLimit = Int
type Validator = (Person Raw -> Either String (Person Checked))

check :: Person Raw -> Person Checked
check Person {..} = Person {..}

hasIds :: Person Raw -> Either String (Person Checked)
hasIds p | null (ids p) = Left $ "No ids " ++ name p
         | otherwise = Right (check p)

validate :: Validator -> ErrorLimit -> [Person Raw] -> Result
validate v limit = snd . foldl fn (0, ([],[])) where
    proceed (errCnt, (errs, checked)) person = case v person of
        Left err -> (succ errCnt, (err:errs, checked))
        Right p -> (errCnt, (errs, p : checked))
    collectErrors acc@(errCnt, (errs, checked)) person = case v person of
        Left err -> (succ errCnt, (err:errs, []))
        _ -> (errCnt, (errs, []))
    fn = liftA3 bool collectErrors proceed ((<= limit) . fst)
        -- if (fst acc) > limit then collectErrors acc else proceed acc 

genPersons :: [Person Raw]        
genPersons = let idss = [[1,2], [], [2], [2,3,4]]
                 names = ["Bob", "Tom", "Kek"]
             in idss >>= \i -> names >>= \n -> return $ Person i n