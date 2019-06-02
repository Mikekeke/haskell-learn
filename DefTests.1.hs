{-# LANGUAGE ConstrainedClassMethods #-}

class Boob a where 
    boob :: Show a => a -> String

instance Boob Int where boob = const "Int"
instance Boob Bool where boob = const "Bool"





