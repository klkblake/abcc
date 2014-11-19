{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module TreeList 
    ( TreeList
    , empty
    , singleton
    , size
    , concat
    , cons
    , fromList
    , toVector
    ) where

import Prelude hiding (concat)

import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data TreeList a = Node (TreeList a) (TreeList a) Int
              | Leaf a
              | Empty

empty :: TreeList a
empty = Empty

singleton :: a -> TreeList a
singleton = Leaf

size :: TreeList a -> Int
size (Node _ _ s) = s
size (Leaf _) = 1
size Empty = 0

concat :: TreeList a -> TreeList a -> TreeList a
concat Empty r = r
concat l Empty = l
concat l r = Node l r (size l + size r)

cons :: a -> TreeList a -> TreeList a
cons x xs = singleton x `concat` xs

fromList :: [a] -> TreeList a
fromList = foldr cons empty

toVector :: TreeList a -> Vector a
toVector xs = runST $ MV.new (size xs) >>= (\v -> go v xs >> return v) >>= V.freeze
  where
    go v (Node l r s) = let s' = size l
                          in go (MV.slice 0 s' v) l >> go (MV.slice s' (s - s') v) r
    go v (Leaf t) = MV.write v 0 t
    go _ Empty = return ()
