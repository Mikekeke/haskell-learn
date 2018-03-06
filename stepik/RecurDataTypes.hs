module RecurDataTypes where

--LIST
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a t) = a : fromList t

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

-- smarter one, not my :(
toList' :: [a] -> List a
toList' = foldr Cons Nil


--NAT
data Nat = Zero | Suc Nat
x1 = Suc (Suc Zero)
x2 = Suc (Suc (Suc Zero))

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero n = n
add (Suc rest) n = Suc (add rest n)

mul :: Nat -> Nat -> Nat
mul n1 n2 = 
    let 
        go _ Zero acc = acc
        go _ (Suc rst) acc = go n1 rst (add n1 acc)
    in
        go n1 n2 Zero

-- better, not my
mul' :: Nat -> Nat -> Nat
mul' Zero _ = Zero
mul' (Suc x) y = add (mul' x y) y
        
fac :: Nat -> Nat
fac Zero = Suc Zero
fac x@(Suc n) = mul x (fac n) 


--TREE
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
tree1 = Leaf 1
tree2 = Node (Leaf 1) (Leaf 1)
tree3 =  Node (Leaf 1) (Node (Leaf 2) (Leaf 2))
tree4 =  Node (Node (Leaf 2) (Leaf 2)) (Node (Node (Leaf 2) (Leaf 2)) (Leaf 2))

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = 1+ (size l) + (size r)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1,x)
    go (Node l r) = (c1+c2, s1+s2) where 
        (c1,s1) = go l
        (c2,s2) = go r
        

--EXPR
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
-- expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (Val a :*: Val b) = Val a :*: Val b
expand (e1 :*: e2) = Val 100
expand e = e

tst = (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
correct = Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))
check = expand tst == expand correct 