-- Lesson 10: try set before reading lesson text
person name age = \f -> f (name, age)
pprint p = p $ \(n,a) -> n ++ ", age " ++ show a 
getName p = p fst
getAge p = p snd
setName name p = p $ \(_, a) -> (\f' -> f' (name, a))
setName' name p = p $ \(_, a) -> person (name, a) -- lections variant
tst =  pprint $ setName "Tom" $ person "Bob" 34