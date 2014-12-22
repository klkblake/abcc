module InterList where

import Prelude hiding (mapM, mapM_)

import Control.Applicative

data InterList a b = Empty !a
                   | Cons !a !b !(InterList a b)
                   deriving Show

empty :: a -> InterList a b
empty = Empty

singleton :: a -> b -> a -> InterList a b
singleton x y x' = Cons x y $ Empty x'

cons :: a -> b -> InterList a b -> InterList a b
cons = Cons

outerList :: InterList a b -> [a]
outerList (Empty x) = [x]
outerList (Cons x _ xys) = x:outerList xys

innerList :: InterList a b -> [b]
innerList (Empty _) = []
innerList (Cons _ y xys) = y:innerList xys

mapM :: Applicative m => (a -> m c) -> (b -> m d) -> InterList a b -> m (InterList c d)
mapM f _ (Empty x) = Empty <$> f x
mapM f g (Cons x y xys) = Cons <$> f x <*> g y <*> mapM f g xys

mapM_ :: Applicative m => (a -> m ()) -> (b -> m ()) -> InterList a b -> m ()
mapM_ f _ (Empty x) = f x
mapM_ f g (Cons x y xys) = f x *> g y *> mapM_ f g xys

reverse :: InterList a b -> InterList a b
reverse il = go (Empty . head $ outerList il) il
  where
    go il' (Empty _)       = il'
    go il' (Cons  _ y xys) = go (Cons (head $ outerList xys) y il') xys
