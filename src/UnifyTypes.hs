module UnifyTypes where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Debug.Trace

import Type
import Op

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let x' = f x
            in if x == x' then x else fixed f x'

rewrite :: String -> Type -> Type -> Type
rewrite v ty (a :*  b)           = rewrite v ty a :*  rewrite v ty b
rewrite v ty (a :+  b)           = rewrite v ty a :+  rewrite v ty b
rewrite v ty (a :~> b)           = rewrite v ty a :~> rewrite v ty b
rewrite _ _  Num                 = Num
rewrite _ _  Unit                = Unit
rewrite v ty (Void a)            = Void $ rewrite v ty a
rewrite v ty (Sealed seal a)     = Sealed seal $ rewrite v ty a
rewrite v ty (Var a) | v == a    = ty
                     | otherwise = Var a
rewrite v ty (Merged a b)        = Merged (rewrite v ty a) (rewrite v ty b)

roll :: Type -> String -> Type -> Type
roll ty v ty' | ty == ty' = Var v
roll ty v (a :*  b)       = roll ty v a :*  roll ty v b
roll ty v (a :+  b)       = roll ty v a :+  roll ty v b
roll ty v (a :~> b)       = roll ty v a :~> roll ty v b
roll _  _ Num             = Num
roll _  _ Unit            = Unit
roll ty v (Void a)        = Void $ roll ty v a
roll ty v (Sealed seal a) = Sealed seal $ roll ty v a
roll _  _ (Var a)         = Var a
roll ty v (Merged a b)    = Merged (roll ty v a) (roll ty v b)

merged :: String -> Type -> Type -> Type
merged v ty (a :*  b)           = merged v ty a :*  merged v ty b
merged v ty (a :+  b)           = merged v ty a :+  merged v ty b
merged v ty (a :~> b)           = merged v ty a :~> merged v ty b
merged _ _  Num                 = Num
merged _ _  Unit                = Unit
merged v ty (Void a)            = Void $ merged v ty a
merged v ty (Sealed seal a)     = Sealed seal $ merged v ty a
merged v ty (Var a) | v == a    = Merged (Var v) ty
                    | otherwise = Var a
merged v ty (Merged a b)        = Merged (merged v ty a) (merged v ty b)

unify :: Type -> Type -> [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unify l r ops = do
    result <- unify' l r
    case result of
        Just f -> let ops' = map (\(Typed ty op) -> Typed (f ty) op) ops
                      (Typed (r' :~> _) _:Typed (_ :~> l') _:_) = ops'
                  in unify l' r' ops'
        Nothing | l == r -> return ops
                | otherwise -> fail $ "INTERNAL ERROR: Failed to unify " ++ show l ++ " and " ++ show r
  where
    a <||> b = do
        a' <- a
        b' <- b
        return $ a' <|> b'

    referenced v (a :*  b) = referenced v a || referenced v b
    referenced v (a :+  b) = referenced v a || referenced v b
    referenced v (a :~> b) = referenced v a || referenced v b
    referenced _ Num = False
    referenced _ Unit = False
    referenced v (Void a) = referenced v a
    referenced v (Sealed _ a) = referenced v a
    referenced v (Var a) = v == a
    referenced v (Merged a b) = referenced v a || referenced v b

    unify' (a :*  b) (c :*  d) = unify' a c <||> unify' b d
    unify' (a :+  b) (c :+  d) = unify' a c <||> unify' b d
    unify' (a :~> b) (c :~> d) = unify' a c <||> unify' b d
    unify' Num       Num       = return Nothing
    unify' Unit      Unit      = return Nothing
    unify' (Void a)  (Void b)  = unify' a b
    unify' (Sealed sealA a) (Sealed sealB b) | sealA == sealB = unify' a b
                                             | otherwise = fail $ "Mismatched seals: " ++ show sealA ++ " /= " ++ show sealB
    unify' (Var a) (Var b) | a == b    = return Nothing
                           | otherwise = do
        cs <- Map.findWithDefault [] b <$> gets tcx_constraints
        modifyConstraints $ Map.delete b
        mapM_ (constrain a) $ trace ("rewrite " ++ show b ++ " " ++ show a) cs
        return . Just $ rewrite b (Var a)
    -- XXX identify recursion
    unify' (Var a) b =
        if referenced a b
            then return . Just $ merged a b . fixed (roll b a)
            else do
                modifyConstraints $ Map.delete a
                trace ("rewrite " ++ show a ++ " " ++ show b) $ return . Just $ rewrite a b
    unify' a (Var b) = unify' (Var b) a
    unify' (Merged a b) (Merged c d) = unify' a c <||> unify' b d
    unify' a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

unifyOps :: [TypedOp] -> TypedOp -> StateT TypeContext (Either String) [TypedOp]
unifyOps [] op = return [op]
-- TODO handle blocks
unifyOps (Typed tyl@(_ :~> tylr) opl:ops) (Typed tyr@(tyrl :~> _) opr) = do x <- unify tylr tyrl $ Typed tyr opr:Typed tyl opl:ops
                                                                            return $ trace ("Current type: " ++ show x) x
unifyOps _ _ = error "called unifyOps on list containing op with non-block type"

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
