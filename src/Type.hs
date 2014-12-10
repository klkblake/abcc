module Type
    ( Type (..)
    , RawType (..)
    ) where

import Control.Monad.State
import Data.Char
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

type ID = Int

data Type = Type ID Bool Bool RawType

data RawType = Product Type Type
             | Sum Type Type
             | Block Type Type
             | Num
             | Unit
             | Void Type
             | Sealed String Type
             | Opaque ID

instance Show Type where
    show ty = evalState (showsType 0 ty) (IS.empty, IM.empty, 0) ""

instance Show RawType where
    show ty = evalState (showsRawType 0 ty . const $ const id) (IS.empty, IM.empty, 0) ""

variables :: [String]
variables = concat . iterate (\vs -> concatMap (\v -> map (v:) vs) ['a'..'z']) $ map (:[]) ['a'..'z']

showsType :: Int -> Type -> State (IS.IntSet, IM.IntMap Int, Int) ShowS
showsType prec (Type ident rel aff ty) = do
    (seen, _, _) <- get
    if IS.member ident seen
        then do
            (_, joins, joinCount) <- get
            put (seen, IM.insert ident joinCount joins, joinCount + 1)
            return . showString . map toUpper $ variables !! joinCount
        else do
            let k = if rel then showChar 'k' else id
            let f = if aff then showChar 'f' else id
                kf = if rel || aff
                         then \l r -> showString l . k . f . showString r
                         else const $ const id
            modify $ \(seen', joins, joinCount) -> (IS.insert ident seen', joins, joinCount)
            ty' <- showsRawType prec ty $ kf
            modify $ \(seen', joins, joinCount) -> (IS.delete ident seen', joins, joinCount)
            (_, joins, _) <- get
            case IM.lookup ident joins of
                Just v  -> return . paren (prec > 0) $ showChar 'μ' . showString (map toUpper $ variables !! v) . showString  ". " . ty'
                Nothing -> return ty'

showsRawType :: Int -> RawType -> (String -> String -> ShowS) -> State (IS.IntSet, IM.IntMap Int, Int) ShowS
showsRawType prec (Product a b) _ = do
    a' <- showsType 8 a
    b' <- showsType 7 b
    return . paren (prec > 7) $ a' . showString " * " . b'

showsRawType prec (Sum a b) _ = do
    a' <- showsType 7 a
    b' <- showsType 6 b
    return . paren (prec > 6) $ a' . showString " + " . b'

showsRawType _ (Block a b) kf = do
    a' <- showsType 0 a
    b' <- showsType 0 b
    return $ showChar '[' . a' . showString " -> " . b' . showChar ']' . kf "" ""

showsRawType _ Num  _ = return $ showChar 'N'
showsRawType _ Unit _ = return $ showChar '1'
showsRawType _ (Void (Type _ _ _ (Opaque _))) kf = return $ showChar '0' . kf "" ""

showsRawType _ (Void a) kf = do
    a' <- showsType 0 a
    return $ showString "0<" . a' . showChar '>' . kf "" ""

showsRawType prec (Sealed seal a) _ = do
    a' <- showsType 9 a
    return . paren (prec > 8) $ showString "Sealed \"" . showString seal . showString "\" " . a'

showsRawType _ (Opaque ident) kf = return $ showString (variables !! ident) . kf "<" ">"

paren :: Bool -> ShowS -> ShowS
paren True  x = showChar '(' . x . showChar ')'
paren False x = x
