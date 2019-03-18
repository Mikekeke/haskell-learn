import Data.Functor.Identity
import Data.Functor.Const

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- view :: ((a -> Const a b) -> s -> Const t) -> (a -> b) -> s -> t
-- view l f = runConst . l (Const . f)

data Person = Person {_name :: String} deriving Show
data Record = Record {_prsn :: Person} deriving Show

nameL :: Functor f => (String -> f String) -> Person -> f Person
nameL k person = fmap (\newName -> person { _name = newName}) (k (_name person))

personL :: Functor f => (Person -> f Person) -> Record -> f Record
personL k record = fmap (\newPers -> record { _prsn = newPers}) (k (_prsn record))


ff = nameL `over` ('G':) 
-- ff = over name ('G':) 
-- ff = over (\k person -> fmap (\newName -> person { _name = newName}) (k (_name person))) ('G':) 
ff1 = runIdentity . (\k person -> fmap (\newName -> person { _name = newName}) (k (_name person))) (Identity . ('G':)) 
ff2 = runIdentity . (\person -> fmap (\newName -> person { _name = newName}) (Identity . ('G':) $ (_name person)))
ff3 = runIdentity . (\person -> fmap (\newName -> person { _name = newName}) (Identity . ('G':) . _name $ person))
p1 = Person "Kek"
ff4 = runIdentity . \person -> fmap (\newName -> person { _name = newName}) (Identity . ('G':) . _name $ person)
ff5 = runIdentity $ fmap (\newName -> p1 { _name = newName}) (Identity . ('G':) . _name $ p1)
ff6 = runIdentity $ fmap (\newName -> p1 { _name = newName}) (Identity "GKek")
ff7 = runIdentity $ Identity . (\newName -> p1 { _name = newName}) $ "GKek"
ff8 = runIdentity $ Identity (Person "GKek")
ff9 = Person "GKek"

