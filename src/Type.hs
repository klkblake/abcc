module Type
    ( Type (..)
    , RawType (..)
    ) where

import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import GraphViz hiding (State)

data Type = Type ID Bool Bool RawType

data RawType = Product Type Type
           | Sum Type Type
           | Block Type Type
           | Num
           | Unit
           | Void Type
           | Sealed String Type
           | Opaque

instance Show Type where
    show ty = evalState (showType 0 ty) (IS.empty, IM.empty, 0)

instance Show RawType where
    show ty = evalState (showRawType 0 ty "") (IS.empty, IM.empty, 0)

variables :: [String]
variables = concat . iterate (\vs -> concatMap (\v -> map (v:) vs) ['a'..'z']) $ map (:[]) ['a'..'z']

showType :: Int -> Type -> State (IS.IntSet, IM.IntMap Int, Int) String
showType prec (Type ident rel aff ty) = do
    (seen, _, _) <- get
    if IS.member ident seen
        then do
            (_, joins, joinCount) <- get
            put (seen, IM.insert ident joinCount joins, joinCount + 1)
            return $ variables !! joinCount
        else do
            let k = if rel then "k" else ""
            let f = if aff then "f" else ""
            modify $ \(seen', joins, joinCount) -> (IS.insert ident seen', joins, joinCount)
            ty' <- showRawType prec ty $ k ++ f
            modify $ \(seen', joins, joinCount) -> (IS.delete ident seen', joins, joinCount)
            (_, joins, _) <- get
            case IM.lookup ident joins of
                Just v  -> return . paren (prec > 0) $ "μ" ++ variables !! v ++ ". " ++ ty'
                Nothing -> return ty'

showRawType :: Int -> RawType -> String -> State (IS.IntSet, IM.IntMap Int, Int) String
showRawType prec (Product a b) _ = do
    a' <- showType 8 a
    b' <- showType 7 b
    return . paren (prec > 7) $ a' ++ " * " ++ b'

showRawType prec (Sum a b) _ = do
    a' <- showType 7 a
    b' <- showType 6 b
    return . paren (prec > 6) $ a' ++ " + " ++ b'

showRawType _ (Block a b) kf = do
    a' <- showType 0 a
    b' <- showType 0 b
    return $ "[" ++ a' ++ " -> " ++ b' ++ "]" ++ kf

showRawType _ Num _ = return "N"
showRawType _ Unit _ = return "1"
showRawType _ (Void (Type _ _ _ Opaque)) kf = return $ "0" ++ kf

showRawType _ (Void a) kf = do
    a' <- showType 0 a
    return $ "0<" ++ a' ++ ">" ++ kf

showRawType prec (Sealed seal a) _ = do
    a' <- showType 9 a
    return . paren (prec > 8) $ "Sealed \"" ++ seal ++ "\"" ++ a'

showRawType _ Opaque kf = return $ "■" ++ kf

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x
