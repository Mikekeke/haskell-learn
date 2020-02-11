{-# LANGUAGE ConstrainedClassMethods #-}
module DefTests1 where
    
import Control.Monad.State
import Data.Map as M
import Control.Monad.State
import Debug.Trace

class Boob a where 
    boob :: Show a => a -> String

instance Boob Int where boob = const "Int"
instance Boob Bool where boob = const "Bool"

-- https://www.hackerrank.com/challenges/common-child/problem?utm_campaign=challenge-recommendation&utm_medium=email&utm_source=60-day-campaign
l1 = "SHINCHAN"
l2 = "NOHARAAA"

-- slow
solve :: String -> String -> Int
solve [] _ = 0
solve _ [] = 0
solve (x:xs) (y:ys)
  | x == y    = 1 + (solve xs ys)
  | otherwise = max (solve (x:xs) ys) (solve xs (y:ys))

f :: Int -> State (M.Map (String, String) Int) Int
f x = modify (M.insert ("s", "s") x) >> return x

ts1 = "APMCTKBUKYRGZPAUVZEBVUXRGDVITOYXWQWRVCSXESMEHQLHPDJQWETAWQVSBRRNRRFDLFTRXOTKQHFTYAZSGBORDNAMUAJTPVOKERLVOLEALDQQLUDCUIRXJHQEZBRWYPFJXNTPELEZHNJILIZVZLYQJDFYSYQNRFFAOYXHQBQVRLFDIIOGWKQIZGVELYOUKZBKMHVYGIKIPSEMWSCWYOJTHOQKMLBAIZYNAKYNCXKDTTESODDAEAHKCDHCJYAHERACMLYQHXIRDFUSRTZDNVHSYFKCSPPYSLHOGIBTNUJTZQWVTHKUNDNWZADMATSUXEISCACQNQXIHNTXGCZUGIGBDONYTUXAXFINAYGZJVDCTZCWPGFNQDPERUCNJUXIFDSQHULYPZRNUOKMLMMQAJMLKCHJMEFJVRYZIPFQOBSDPAITHGMNKROCWJEGESCGOIUOQHOYUEQNPJPBMCNRZUHOSQNSUNCSTVQVWFGMUFJZGMEUVUPH"
ts2 = "JUVSDRRSHFGSSLLLZEPJDVAWDPKQBKUHHOZFFXKQMGAACZUYOMNPHWGTYZWQGSMNYXWNFYNOIVVMPZXUNKJQYBYJINBOHXUWIVRTVLEKCOPDMTKTGDBWECDAVPMLHQLERZHDVZJZODPSAPGSRWJXNGFEBQBLTLNDIEGFHEGHJWFOIYXRUJMODSNXUFWBIJJMXTFMUKQEYPNBTZFEJNLDNWCGQLVUQUKGZHJOKZNPMUYEQLEYNNORKJQAMSTHTBCCPQTTCPRZATWNJQJXPODRXKIWDOFUBZVSDTAPFRMXJBJMUGVRZOCDUIPXVEGMRQNKXDKNWXMTNDJSETAKVSYMJISAREEJPLRABMXJSRQNASOJNEEVAMWCFJBCIOCKMHCMYCRCGYFNZKNALDUNPUSTSWGOYHOSWRHWSMFGZDWSBXWXGVKQPHGINRKMDXEVTNNZTBJPXYNAXLWZSBUMVMJXDIKORHBIBECJNKWJJJSRLYQIKKPXSNUT"

-- still slow for stuff like ts1 & ts2
solveST :: String -> String -> State (M.Map (String, String) Int) Int
solveST [] _ = return 0
solveST _ [] = return 0
solveST s1@(x:xs) s2@(y:ys) 
    | x == y = do
        r <- solveST xs ys
        let res = r + 1
        modify (M.insert (s1, s2) res) 
        return res
    | otherwise = do
        seen <- gets (M.lookup (s1,s2))
        case seen of
            Just v -> return v
            Nothing -> do
                r1 <- solveST (x:xs) ys
                r2 <- solveST xs (y:ys)
                let res = max r1 r2
                modify (M.insert (s1, s2) res)
                return res



data SyncRes = SuncSuccess | SyncFail {errors :: [String]}

pres :: SyncRes -> String
pres sr = show (errors sr)