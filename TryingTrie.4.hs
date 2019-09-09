import Data.Map.Strict as M
import Control.Monad
import Debug.Trace
-- import Data.List.NonEmpty
-- import Data.List



newtype Trie a = Trie {getMap :: Map a (Trie a)}

instance Show a => Show (Trie a) where
    -- show (Trie t) = foldMap (\(k,t') -> k :": {" ++ show t' ++ "}") (M.toList t)
    show (Trie t) = show (M.toList t)

emptyT = Trie M.empty
find a = M.lookup a . getMap

add :: String -> Trie Char -> Trie Char
add [] _ = emptyT
add (c:cs) t = case M.lookup c (getMap t) of 
    Just tmap' -> Trie $ M.adjust (add cs) c (getMap t)
    Nothing -> Trie $ M.insert c (add cs t) (getMap t)

testT = add "dog" . add "dot" $ emptyT
testFind = find 'd' >=> find 'o'

longetsSuff :: String -> Trie Char ->  String
longetsSuff [] t = ""
longetsSuff (c:cs) t = case M.lookup c (getMap t) of
    Just t' -> c : longetsSuff cs t'
    Nothing -> ""

lSuff :: Trie Char -> String -> String
lSuff t [] = ""
lSuff t (c:cs) = case M.lookup c (getMap t) of
    Just t' -> c : lSuff t' cs
    Nothing -> ""

--doesnt work as expected, can write as fold??
lSuff1 s t = snd $ Prelude.foldr (\c (t1, acc1) -> undefined) (t,"") (reverse s) where
    f c' tt = case M.lookup c' (getMap tt) of
        Just t' -> trace (show c' ++ " -> just") (t', (c' :))
        Nothing -> undefined

mp = add "dog" . add "dot" . add "dororo" $ emptyT
tst1 = lSuff1 "doroga" mp

restore :: Trie Char -> [String]
restore = go [] [] where 
    go :: [String] -> String -> Trie Char -> [String]
    go acc path (Trie m) | M.null m = (reverse path) : acc
                         | otherwise = do
                            key <- keys m
                            go acc (key:path) (m ! key)

restore2 :: Trie Char -> [String]
restore2 = go [] id where 
    go :: [String] -> (String -> String) -> Trie Char -> [String]
    go acc f (Trie m) | M.null m = (f []) : acc
                      | otherwise = do
                            key <- keys m
                            go acc (f . (key :)) (m ! key)

-- learn QuckCheck
testAll = mapM_ (putStrLn . show) $
    [
        ("retires equality", restore mp == restore2 mp)
    ]