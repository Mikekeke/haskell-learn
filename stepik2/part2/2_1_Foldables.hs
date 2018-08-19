import           Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldl f b (Tr x y z) = f(f(f b x)y)z
    foldr f b (Tr x y z) = f x (f y (f z b))
    --some more variants
    -- foldr f ini (Tr x y z) = foldr f ini [x, y, z]
    -- foldl f ini (Tr x y z) = foldl f ini [x, y, z]
    -- foldl f s (Tr x y z) = s `f` x `f` y `f` z


-- instance Foldable Triple where
--     foldMap f (Tr x y z) = f x <> f y <> f z
    -- info
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- https://en.wikibooks.org/wiki/Haskell/Foldable


-- Да, все 6 вариантов можно легко увидеть, если переписать выражения из лекции немного в другой форме. Например, для inorder:

-- instance Foldable Tree where
--   foldr f ini Nil = ini
--   foldr f ini (Branch l x r) = (\i -> (foldr f i l)) . f x . (\i -> (foldr f i r)) $ ini

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

-- inorder
instance Foldable Tree where
    foldr f ini Nil            = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l
    -- foldr (:) [] tree ~> [1,2,3,4]
    -- foldr f ini (Branch l x r) = (\i -> (foldr f i l)) . f x . (\i -> (foldr f i r)) $ ini

instance Foldable Preorder where
    foldr f ini (PreO Nil)     = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))
    -- foldr (:) [] $ PreO tree ~> [3,1,2,4]
    -- foldr f ini (LevelO (Branch l x r)) =
    --     f x . (\i -> (foldr f i (LevelO l))) . (\i -> (foldr f i (LevelO r))) $ ini

instance Foldable Postorder where
    foldr f ini (PostO Nil)     = ini
    foldr f ini (PostO (Branch l x r)) =  foldr f (foldr f (f x ini) (PostO r)) (PostO l)
    -- foldr (:) [] $ PreO tree ~> [2,1,4,3]
    -- foldr f ini (LevelO (Branch l x r)) =
    --     (\i -> (foldr f i (LevelO l))) . (\i -> (foldr f i (LevelO r))) . f x $ ini

toLs :: Levelorder a -> [a]
toLs (LevelO Nil)            = []
toLs (LevelO (Branch Nil x Nil)) = [x]
toLs (LevelO (Branch Nil x r)) = [x] ++ (toLs $ LevelO r)
toLs (LevelO (Branch l x Nil)) = [x] ++ (toLs $ LevelO l)
toLs (LevelO (Branch l x r)) =  [x,l',r'] ++ ls ++ rs where
    (l':ls) = toLs $ LevelO l
    (r':rs) = toLs $ LevelO r

instance Foldable Levelorder where
    foldr f ini (LevelO Nil)             = ini
    foldr f ini  (LevelO (Branch l x r)) = f x $ foldr f ini ([LevelO l, LevelO r])
    -- [3,1,4,2]

