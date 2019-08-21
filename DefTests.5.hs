import Control.Monad
import Debug.Trace


tk :: (a -> Bool) -> [a] -> [a]
tk p l = foldr (\x b -> (f x) b) [] l where
    f a = if p a then trace "call" (a:) else  const []

{-
tk (==1) [1,2]
-}


