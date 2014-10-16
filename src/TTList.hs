{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module TTList 
    ( Tagged (..)
    , TTList
    , empty
    , singleton
    , size
    , concat
    , cons
    , toVector
    ) where

import Prelude hiding (concat)

import Control.Monad.ST
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

class Monoid (Tag a) => Tagged a where
    type Tag a
    tag :: a -> Tag a

data TTList a = Node (TTList a) (TTList a) Int (Tag a)
              | Leaf a (Tag a)
              | Empty

instance Tagged a => Monoid (TTList a) where
    mempty = empty
    mappend = concat

instance Tagged a => Tagged (TTList a) where
    type Tag (TTList a) = Tag a
    tag (Node _ _ _ t) = t
    tag (Leaf _ t) = t
    tag Empty = mempty

empty :: TTList a
empty = Empty

singleton :: Tagged a => a -> TTList a
singleton x = Leaf x $ tag x

size :: TTList a -> Int
size (Node _ _ s _) = s
size (Leaf _ _) = 1
size Empty = 0

concat :: Tagged a => TTList a -> TTList a -> TTList a
concat Empty r = r
concat l Empty = l
concat l r = Node l r (size l + size r) (tag l <> tag r)

cons :: Tagged a => a -> TTList a -> TTList a
cons x xs = singleton x `concat` xs

toVector :: TTList a -> Vector a
toVector xs = runST $ MV.new (size xs) >>= (\v -> go v xs >> return v) >>= V.freeze
  where
    go v (Node l r s _) = let s' = size l
                          in go (MV.slice 0 s' v) l >> go (MV.slice s' (s - s') v) r
    go v (Leaf t _) = MV.write v 0 t
    go _ Empty = return ()
