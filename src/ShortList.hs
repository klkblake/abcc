{-# LANGUAGE BangPatterns #-}
module ShortList
    ( ShortList (..)
    , (!)
    , update
    , fromList
    , toList
    ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- We manually track the length instead of using different constructors
-- since GHC can't UNPACK the latter.
data ShortList a = ShortList {-# UNPACK #-} !Int a a a

err :: Int -> Int -> a
err i n = error $ "Attempted to access non-existant index " ++ show i ++ " of " ++ show n

err00, err10, err20, err11, err21, err22 :: a
err00 = err 0 0
err10 = err 1 0
err20 = err 2 0
err11 = err 1 1
err21 = err 2 1
err22 = err 2 2

instance Functor ShortList where
    fmap _ (ShortList 0 _ _ _) =    ShortList 0 err00 err10 err20
    fmap f (ShortList 1 a _ _) = let !fa = f a
                                 in ShortList 1 fa    err11 err21
    fmap f (ShortList 2 a b _) = let !fa = f a
                                     !fb = f b
                                 in ShortList 2 fa    fb    err22
    fmap f (ShortList 3 a b c) = let !fa = f a
                                     !fb = f b
                                     !fc = f c
                                 in ShortList 3 fa    fb    fc
    fmap _ _ = error "Invalid ShortList"

instance Foldable ShortList where
    foldMap _ (ShortList 0 _ _ _) = mempty
    foldMap f (ShortList 1 a _ _) = f a
    foldMap f (ShortList 2 a b _) = f a <> f b
    foldMap f (ShortList 3 a b c) = f a <> f b <> f c
    foldMap _ _ = error "Invalid ShortList"

instance Traversable ShortList where
    traverse _ (ShortList 0 _ _ _) = pure $            ShortList 0 err00 err10 err20
    traverse f (ShortList 1 a _ _) = (\ !fa         -> ShortList 1 fa    err11 err21) <$> f a
    traverse f (ShortList 2 a b _) = (\ !fa !fb     -> ShortList 2 fa    fb    err22) <$> f a <*> f b
    traverse f (ShortList 3 a b c) = (\ !fa !fb !fc -> ShortList 3 fa    fb    fc)    <$> f a <*> f b <*> f c
    traverse _ _ = error "Invalid ShortList"

(!) :: ShortList a -> Int -> a
(!) (ShortList _ a _ _) 0 = a
(!) (ShortList _ _ b _) 1 = b
(!) (ShortList _ _ _ c) 2 = c
(!) (ShortList n _ _ _) i = err i n
{-# INLINABLE (!) #-}

update :: Int -> a -> ShortList a -> ShortList a
update 0 !a (ShortList n _ b c) | n > 0 = ShortList n a b c
update 1 !b (ShortList n a _ c) | n > 1 = ShortList n a b c
update 2 !c (ShortList n a b _) | n > 2 = ShortList n a b c
update i  _ (ShortList n _ _ _) = err i n
{-# INLINABLE update #-}

fromList :: [a] -> ShortList a
fromList []           = ShortList 0 err00 err10 err20
fromList [!a]         = ShortList 1 a     err11 err21
fromList [!a, !b]     = ShortList 2 a     b     err22
fromList [!a, !b, !c] = ShortList 3 a     b     c
fromList _ = error "Attempted to create ShortList longer than 3 elements"
{-# INLINABLE fromList #-}
