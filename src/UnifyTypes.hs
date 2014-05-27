module UnifyTypes where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Debug.Trace

import Type
import Op

unify :: Type -> Type -> [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unify l r ops = do
    result <- unify' l r
    case result of
        Just f -> let ops' = map (\(Typed ty op) -> Typed (f ty) op) ops
                      (Typed (r' :~> _) _:Typed (_ :~> l') _:_) = ops'
                  in unify l' r' ops'
        Nothing | l == r -> return ops
                | otherwise -> fail $ "Could not unify " ++ show l ++ " and " ++ show r
  where
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

    a <||> b = do
        a' <- a
        b' <- b
        return $ a' <|> b'

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
        mapM_ (constrain a) cs
        trace ("rewrite " ++ show b ++ " " ++ show a) $ return . Just $ rewrite b (Var a)
    -- XXX identify recursion
    unify' (Var a) b = do
        modifyConstraints $ Map.delete a
        trace ("rewrite " ++ show a ++ " " ++ show b) $ return . Just $ rewrite a b
    unify' a (Var b) = unify' (Var b) a
    unify' (Merged a b) (Merged c d) = unify' a c <||> unify' b d
    unify' a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

unifyOps :: [TypedOp] -> TypedOp -> StateT TypeContext (Either String) [TypedOp]
unifyOps [] op = return [op]
-- TODO handle blocks
unifyOps (Typed tyl@(_ :~> tylr) opl:ops) (Typed tyr@(tyrl :~> _) opr) = unify tylr tyrl $ Typed tyr opr:Typed tyl opl:ops
unifyOps _ _ = error "called unifyOps on list containing op with non-block type"

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
