{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Identity

-- "Higher-Kinded Data"
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data Person' f = Person
  { pName :: HKD f String
  , pAge  :: HKD f Int
  }

-- can use alias type Person = Person' Identity
validate :: Person' Maybe -> Maybe (Person' Identity)
validate (Person name age) =
  Person <$> name <*> age

test = pName <$> validate (Person (Just "Bob") (Just 22))