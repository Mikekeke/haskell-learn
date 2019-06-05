import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad
import Data.Monoid

getTime :: IO String
getTime = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return $ show hour ++ ":" ++ show minute ++ ":" ++ (show second) ++ "; "

newtype Timenada a = Timenada {runTime :: IO (a, String)} -- hidden

instance Functor Timenada where
    fmap = liftM

instance Applicative Timenada where
    pure = return
    (<*>) = ap

instance Monad Timenada where
    return a = Timenada $ pure (a,"")
    m >>= k = Timenada $ do
        (a, times) <- runTime m
        (a1, times') <- runTime (k a)
        return $ (a1, times' <> times) -- lookslike writer's memory leak

timed a = Timenada $ (\t -> (a,t)) <$> getTime

test :: Timenada String
test = do
    x <- timed $ take 3 "test"
    y <- timed "test2"
    return (x ++ y)

test2 :: Timenada String
test2 = timed "lol"