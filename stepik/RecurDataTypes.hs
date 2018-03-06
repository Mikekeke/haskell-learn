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
size (Leaf _) = 0
size (Node l r) = 1+ (size l) + (size r)

