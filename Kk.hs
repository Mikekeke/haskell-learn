{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- import Data.Aeson
-- import GHC.Generics (Generic)
-- import Data.Text
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import Data.List
-- import Data.Monoid

-- input = "[{id:1,name:\"Андрей\", age: 11}, {id:2,name:\"Иван\", age: 14}, {id:3,name:\"Петр\", age: 13}, {id:4,name:\"Сергей\", age: 90}, {id:5,name:\"Марина\", age: 24}, ]"
-- input2 = "[{\"id\":1, \"name\":\"Андрей\", \"age\": 20}, {\"id\":1, \"name\":\"Андрей\", \"age\": 11}]"

-- data Person = Person {id :: Int, name :: !Text, age :: Int} deriving (Show, Generic)


-- validate p@(Person _ name age) | age < 0 || age > 120 = Left  ("Ebanulsa? " <> show age <> " let?")
--                           | otherwise = Right p
-- instance FromJSON Person


-- tst = Data.List.partition ((> 17) . age) <$> maybe (Left "ne rasparsil") (traverse validate) (decode (BS.pack input2) :: Maybe [Person])

rv :: [a] -> [a]
rv xs = foldr 
    (\x b -> b . (x:)) 
    -- (\x b acc -> b (x:acc)) 
    -- (\x b -> \acc -> b (x:acc)) -- SO variant
    -- (\x -> (. (x:))) 
    id xs []




