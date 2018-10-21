{-# LANGUAGE TypeOperators #-}

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

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq)
newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Show a => Show (Tree a) where 
    show Nil = "(X)"
    show (Branch l a r) = (show l) ++ "-" ++ (show a) ++ "-" ++ (show r)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
treeLection = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

-- inorder
instance Foldable Tree where
    foldr f ini Nil            = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l
    -- foldr (:) [] tree ~> [1,2,3,4]
    -- foldr f ini (Branch l x r) = (\i -> (foldr f i l)) . f x . (\i -> (foldr f i r)) $ ini

instance Foldable Preorder where
    foldr f ini (PreO Nil)     = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini r') l') where
        l' = PreO l
        r' = PreO r
    -- foldr (:) [] $ PreO tree ~> [3,1,2,4]
    -- foldr f ini (PreO (Branch l x r)) =
    --     f x . (\i -> (foldr f i (PreO l))) . (\i -> (foldr f i (PreO r))) $ ini

instance Foldable Postorder where
    foldr f ini (PostO Nil)     = ini
    foldr f ini (PostO (Branch l x r)) =  foldr f (foldr f (f x ini) r') l' where
        l' = PostO l
        r' = PreO r
    -- foldr (:) [] $ PostO tree ~> [2,1,4,3]
    -- foldr f ini (PostO (Branch l x r)) =
    --     (\i -> (foldr f i (PostO l))) . (\i -> (foldr f i (PostO r))) . f x $ ini


getChildren :: Tree a -> [Tree a]
getChildren (Branch Nil _ Nil) = []
getChildren (Branch Nil _ r) = [r]
getChildren (Branch l _ Nil) = [l]
getChildren (Branch l _ r) = [l,r]

value (Branch _ x _) = x

childs :: [Tree a] -> [Tree a]
childs [] = []
childs ts = let res = concatMap getChildren ts in ts ++ childs res

levelOrder :: Tree a -> [Tree a]
levelOrder t = childs [t]

instance Foldable Levelorder where
    foldr _ ini (LevelO Nil) = ini
    foldr f ini (LevelO t)   = foldr f ini $ value <$> levelOrder t

-- !!! awesome short from solutions
-- instance Foldable Levelorder where    
--     foldr f ini (LevelO tree) = levelorder [tree] where
--         levelorder [] = ini
--         levelorder (Nil:xs) = levelorder xs
--         levelorder ((Branch l x r):xs) = f x (levelorder (xs ++ [l,r]))

-- more good
-- instance  Foldable Levelorder  where
--     foldr f b (LevelO tree) = go b [tree] where
--       go acc [] = acc
--       go acc (t:ts) = case t of
--         Nil -> go b ts
--         Branch l a r -> f  a (go acc (ts++[l,r]))

-- -----------------------------------------------------
-- next Nil = []
-- next (Branch l v r) = [l, r]

-- value Nil = []
-- value (Branch l v r) = [v]

-- instance Foldable Levelorder where
--   foldr f x (LevelO tree) = let
--      calc [] = x
--      calc level = foldr f (calc (level >>= next)) (level >>= value)
--      in calc [tree]

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldr (mappend . Endo) (Endo id)
-- or
-- mkEndo = foldMap Endo
-- or
-- mkEndo = Endo . foldr (.) id 

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)
-- length $ Cmps [[1,2], [], [3,4,5,6,7]]
instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap fn = foldMap (foldMap fn) . getCmps
    -- or
    -- foldr f ini = foldr (\ga b -> foldr f b ga) ini . getCmps 
    -- shorter 
    foldr f ini = foldr (flip $ foldr f) ini . getCmps
    
    -- from solutions
    -- instance (Foldable a, Foldable b)=>Foldable (a |.| b) where foldMap = (. getCmps).foldMap.foldMap