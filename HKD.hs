{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Identity
import Data.Monoid

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data User' f = User {name_ :: HKD f String, age_ :: HKD f Integer}
type User = User' Identity
instance Show User where
    show (User nameK ageK) = "User " ++ show nameK ++ " " ++ show ageK

newtype Diff a = Diff{diff :: Maybe a}

initUser :: User
initUser = User "Bob"  33
diffUser :: User' Diff
diffUser = User (Diff Nothing) (Diff (Just 12))

merge :: User -> User' Diff -> User
merge (User name age ) (User difName difAge) = User (merge' name difName) (merge' age difAge) where
    merge' :: p -> Diff p -> p
    merge' _ (Diff (Just a)) = a
    merge' a (Diff Nothing) = a