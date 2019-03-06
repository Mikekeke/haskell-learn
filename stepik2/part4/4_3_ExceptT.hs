{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Class
import Control.Applicative
import Data.Char (isNumber, isPunctuation)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Control.Monad
import Data.Semigroup


data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile

availableMoves :: Point -> [Point]
availableMoves (x,y) = [succ, pred] >>= \f -> [(f x, y), (x, f y)]

move :: GameMap -> Point -> Either DeathReason Point
move m p = case m p of
    Floor -> Right p 
    Chasm -> Left Fallen
    Snake -> Left Poisoned

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m 0 p = [move m p]
moves m n p = do
    nextPoint <- availableMoves p
    case move m nextPoint of
            Right p' -> (moves m (n-1) nextPoint) 
            Left r -> Left r : []

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie rsn mp n pt = foldl (\b -> (b +) . either (fromEnum . (rsn ==)) (const 0)) 0 (moves mp n pt)


map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

-- stuffs solution

moves' :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves' m n p = runExceptT $ foldr (>=>) pure (replicate n (ExceptT . move')) p
  where
    steps :: Point -> [Point]
    steps (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    stepOn :: Point -> Either DeathReason Point
    stepOn p = case m p of
      Floor -> Right p
      Chasm -> Left Fallen
      Snake -> Left Poisoned

    move' :: Point -> [Either DeathReason Point]
    move' p = stepOn <$> steps p

-- *****************************************************
{-
askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s
-}

newtype PwdError = PwdError String deriving (Show, Semigroup, Monoid)

type PwdErrorIOMonad = ExceptT PwdError IO

incorrectInpErr :: String -> PwdError
incorrectInpErr = PwdError . ("Incorrect input: " ++)

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

validate pass | length pass <= 8 = Left . incorrectInpErr $ "password is too short!"
              | not $ any isNumber pass = Left . incorrectInpErr $  "password must contain some digits!"
              | not $ any isPunctuation pass = Left . incorrectInpErr $ "password must contain some punctuation!"
              | otherwise = pure pass

getValidPassword :: PwdErrorIOMonad String
getValidPassword = action `catchE` (\err@(PwdError s) -> lift (putStrLn s) >> throwE err)
    where action = do
            pass <- liftIO getLine
            case validate pass of
                Left (PwdError s) -> throwE (PwdError s)
                (Right validatedPass) -> return validatedPass
