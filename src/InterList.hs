module InterList where

data InterList a b = Empty a
                   | Cons a b (InterList a b)

empty :: a -> InterList a b
empty = Empty

singleton :: a -> b -> a -> InterList a b
singleton x y x' = Cons x y $ Empty x'

cons :: a -> b -> InterList a b -> InterList a b
cons = Cons

outerList :: InterList a b -> [a]
outerList (Empty x) = [x]
outerList (Cons x _ xys) = x:outerList xys

reverse :: InterList a b -> InterList a b
reverse il = go (Empty . head $ outerList il) il
  where
    go il' (Empty _)       = il'
    go il' (Cons  _ y xys) = Cons (head $ outerList xys) y il'
