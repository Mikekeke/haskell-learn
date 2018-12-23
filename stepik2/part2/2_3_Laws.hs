{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Function
import Data.Monoid
import Data.Traversable (foldMapDefault, fmapDefault)

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3

instance Functor OddC where
    fmap f (Un a) = Un $ f a
    fmap f (Bi a1 a2 o) = on Bi f a1 a2 (fmap f o)

instance Foldable OddC where
    foldr f x (Un a) = f a x
    foldr f x (Bi a1 a2 o) = f a1 (f a2 (foldr f x o))

    foldMap f (Un a) = f a
    foldMap f (Bi a1 a2 o) = f a1 <> f a2 <> foldMap f o

instance Traversable OddC where
    traverse fa (Un a) = Un <$> fa a
    traverse fa (Bi a1 a2 o) = Bi <$> fa a1 <*> fa a2 <*> traverse fa o

    sequenceA (Un a) = fmap Un a
    sequenceA (Bi a1 a2 o) = Bi <$> a1 <*> a2 <*> sequenceA o


-- **********************************************
{-
Сделайте двоичное дерево
представителем класса типов Traversable таким образом, 
чтобы обеспечить для foldMapDefault порядок обхода «postorder traversal»

foldMapDefault = getConst . traverse (Const . f)
Const f <*> Const v = Const (f <> v)
-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil            = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldMap = foldMapDefault
    -- foldMap _ Nil = mempty
    -- foldMap f (Branch l x r) = foldMap f l `mappend` f x `mappend` foldMap f r -- так всё равно будет in order


instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = (flip . Branch) <$> traverse f l <*> traverse f r <*> f x

  sequenceA Nil = pure Nil
  sequenceA (Branch l x r) = (flip <$> (Branch <$> sequenceA l)) <*> sequenceA r <*> x

--   из решений
--   sequenceA (Branch l x r) = (flip <$> Branch) <$> sequenceA l <*> sequenceA r <*> x
--   sequenceA (Branch l x r) = (flip.Branch) <$> sequenceA l <*> sequenceA r <*> x
--                                  ^^^^^^  сначала в Branch передается первый аргумент, а потом меняет местами второй и третий аргумент

-- foldMapDefault = getConst . traverse (Const . f)
-- Const f <*> Const v = Const (f <> v)
-- контейнер пересобирается как был, но меняется порядок выполнения эффектов,
-- потому что так написан foldMapDefault и аппликатив для Const, a foldMap = foldMapDefault
-- если написать foldMap как в WrapTree (а там in-order, см. ниже), то перестановка эффектов в sequenceA изменений не вызовет и останется in-order



testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
-- testTree = Branch Nil 1 Nil
test1 = foldMapDefault (\x -> [x]) testTree
test1_2 = foldMap (\x -> [x]) testTree


{-
для подобных случаев есть полезное расширение, позволяющее пользоваться do-нотацией для Applicative. Здесь мы явно указываем, в каком порядке будут выполняться эффекты. Сначала пытался сдать именно такое решение, но система Степика его не приняла, видимо расширения включать нельзя. Тем не менее, оно полностью рабочее, можете проверить на своём компиляторе.

# LANGUAGE ApplicativeDo #

instance Traversable Tree where
    traverse f Nil            = pure Nil
    traverse f (Branch l c r) = do
        la <- traverse f l
        ra <- traverse f r
        ca <- f c
        pure (Branch la ca ra)
-}


data WrapTree a = WrapTree {unWrap :: Tree a} deriving (Eq, Show)

instance Functor WrapTree where
    fmap _ (WrapTree Nil)            = WrapTree Nil
    fmap f (WrapTree (Branch l x r)) = WrapTree $ Branch (fmap f l) (f x) (fmap f r)

instance Foldable WrapTree where
    foldMap _ (WrapTree Nil) = mempty          
    foldMap f (WrapTree (Branch l x r)) = foldMap f (WrapTree l) `mappend` f x `mappend` foldMap f (WrapTree r)

instance Traversable WrapTree where
  sequenceA (WrapTree Nil) = pure ( WrapTree Nil)
  sequenceA (WrapTree tree) =  WrapTree <$> sequenceA tree

testWrapTree = WrapTree $ Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
test2 = foldMap (\x -> [x]) testWrapTree