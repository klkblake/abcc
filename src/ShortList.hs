{-# LANGUAGE BangPatterns #-}
module ShortList
    ( ShortList (..)
    , (!)
    , update
    , fromList
    , toList
    ) where

-- We manually track the length instead of using different constructors
-- since GHC can't UNPACK the latter.
data ShortList a = ShortList {-# UNPACK #-} !Int a a a

err :: Int -> Int -> a
err i n = error $ "Attempted to access non-existant index " ++ show i ++ " of " ++ show n

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
fromList []           = ShortList 0 (err 0 0) (err 1 0) (err 2 0)
fromList [!a]         = ShortList 1 a         (err 1 1) (err 2 1)
fromList [!a, !b]     = ShortList 2 a         b         (err 2 2)
fromList [!a, !b, !c] = ShortList 3 a         b         c
fromList _ = error "Attempted to create ShortList longer than 3 elements"
{-# INLINABLE fromList #-}

toList :: ShortList a -> [a]
toList (ShortList 0 _ _ _) = []
toList (ShortList 1 a _ _) = [a]
toList (ShortList 2 a b _) = [a, b]
toList (ShortList 3 a b c) = [a, b, c]
toList _ = error "Invalid ShortList"
{-# INLINABLE toList #-}
