module UnifyTypes where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Debug.Trace

import Type
import Op

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let x' = f x
            in if x == x' then x else fixed f x'

normalise :: Type -> Type -> Type
normalise (a :*  b) (c :*  d) = normalise a c :* normalise b d
normalise (a :+  b) (c :+  d) = normalise a c :+ normalise b d
normalise (Block aff rel a b) (Block aff' rel' c d) = Block (normaliseFE aff aff') (normaliseFE rel rel') (normalise a c) (normalise b d)
normalise Num Num = Num
normalise Unit Unit = Unit
normalise (Void a) (Void b) = Void $ normalise a b
normalise (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA $ normalise a b
normalise (Fix a b) (Fix c d) | a == c = Fix a $ normalise b d
normalise (Var a) (Var b) | a == b = Var a
normalise (Merged a b) (Merged c d) =
    let x = normalise a b
        y = normalise c d
    in case (x, y) of
           (Merged _ _, _) -> Merged x y
           (_, Merged _ _) -> Merged x y
           _ -> normalise x y
normalise a b = Merged a b

normaliseFE :: FlagExpr -> FlagExpr -> FlagExpr
normaliseFE (FVar a)  (FVar b) | a == b = FVar a
normaliseFE (FLit a)  (FLit b)          = FLit $ a || b
normaliseFE (FOr a b) (FOr c d) =
    let x = normaliseFE a b
        y = normaliseFE c d
    in case (x, y) of
           (FOr _ _, _) -> FOr x y
           (_, FOr _ _) -> FOr x y
           _ -> normaliseFE x y
normaliseFE (FAffine   ty) (FAffine   ty') = FAffine   $ normalise ty ty'
normaliseFE (FRelevant ty) (FRelevant ty') = FRelevant $ normalise ty ty'
normaliseFE a b = FOr a b

mapTy :: (Type -> Maybe Type) -> (FlagExpr -> Maybe FlagExpr) -> Type -> Type
mapTy f _ ty | Just ty' <- f ty = ty'
mapTy f g (a :*  b)           = mapTy f g a :*  mapTy f g b
mapTy f g (a :+  b)           = mapTy f g a :+  mapTy f g b
mapTy f g (Block aff rel a b) = Block (mapTyFE f g aff) (mapTyFE f g rel) (mapTy f g a) (mapTy f g b)
mapTy _ _ Num                 = Num
mapTy _ _ Unit                = Unit
mapTy f g (Void a)            = Void $ mapTy f g a
mapTy f g (Sealed seal a)     = Sealed seal $ mapTy f g a
mapTy f g (Fix v a)           = Fix v $ mapTy f g a
mapTy _ _ (Var a)             = Var a
mapTy f g (Merged a b)        = normalise (mapTy f g a) (mapTy f g b)

mapTyFE :: (Type -> Maybe Type) -> (FlagExpr -> Maybe FlagExpr) -> FlagExpr -> FlagExpr
mapTyFE _ g fe | Just fe' <- g fe = fe'
mapTyFE _ _ (FVar var) = FVar var
mapTyFE _ _ (FLit v)   = FLit v
mapTyFE f g (FOr  a b) = FOr (mapTyFE f g a) (mapTyFE f g b)
mapTyFE f g (FAffine   ty) = FAffine   $ mapTy f g ty
mapTyFE f g (FRelevant ty) = FRelevant $ mapTy f g ty

rewrite :: String -> Type -> Type -> Type
rewrite v ty = mapTy rewrite' $ const Nothing
  where
    rewrite' (Fix v' a) | v == v', Var b <- ty = Just . Fix b $ rewrite v ty a
    rewrite' (Fix v' _) | v == v' = error "Attempted to rewrite var in Fix"
    rewrite' (Var a) | v == a = Just ty
    rewrite' _ = Nothing

rewriteFE :: String -> FlagExpr -> Type -> Type
rewriteFE v fe = mapTy (const Nothing) rewriteFE'
  where
    rewriteFE' (FVar a) | v == a = Just fe
    rewriteFE' _ = Nothing

roll :: Type -> String -> Type -> Type
roll ty v = mapTy roll' $ const Nothing
  where
    roll' ty' | ty == ty' = Just $ Var v
    roll' _ = Nothing

unify :: Type -> Type -> [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unify l r ops = do
    result <- unify' l r
    case result of
        Just f -> let ops' = map (\(Typed tyl tyr op) -> Typed (f tyl) (f tyr) op) ops
                      (Typed r' _ _:Typed _ l' _:_) = ops'
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
    referenced v (Block aff rel a b) = referencedFE v aff || referencedFE v rel || referenced v a || referenced v b
    referenced _ Num = False
    referenced _ Unit = False
    referenced v (Void a) = referenced v a
    referenced v (Sealed _ a) = referenced v a
    referenced v (Fix _ a) = referenced v a
    referenced v (Var a) = v == a
    referenced v (Merged a b) = referenced v a || referenced v b

    referencedFE v (FVar var) = v == var
    referencedFE _ (FLit _)   = False
    referencedFE v (FOr  a b) = referencedFE v a || referencedFE v b
    referencedFE v (FAffine   ty) = referenced v ty
    referencedFE v (FRelevant ty) = referenced v ty

    unify' (a :*  b) (c :*  d) = unify' a c <||> unify' b d
    unify' (a :+  b) (c :+  d) = unify' a c <||> unify' b d
    unify' (Block aff rel a b) (Block aff' rel' c d) = unifyFE aff aff' <||> unifyFE rel rel' <||> unify' a c <||> unify' b d
    unify' Num       Num       = return Nothing
    unify' Unit      Unit      = return Nothing
    unify' (Void a)  (Void b)  = unify' a b
    unify' (Sealed sealA a) (Sealed sealB b) | sealA == sealB = unify' a b
                                             | otherwise = fail $ "Mismatched seals: " ++ show sealA ++ " /= " ++ show sealB
    unify' (Fix a b) (Fix c d) = unify' (Var a) (Var c) <||> unify' b d
    unify' (Var a) (Var b) | a == b    = return Nothing
                           | otherwise = do
        cs <- Map.findWithDefault [] b <$> gets tcx_constraints
        modifyConstraints $ Map.delete b
        mapM_ (constrain a) $ trace ("rewrite " ++ show b ++ " " ++ show a) cs
        return . Just $ rewrite b (Var a)
    unify' (Var a) b =
        if referenced a b
            then trace ("fix " ++ show a ++ " " ++ show b) $ return . Just $ rewrite a (Fix a b) . fixed (roll b a)
            else do
                modifyConstraints $ Map.delete a
                trace ("rewrite " ++ show a ++ " " ++ show b) $ return . Just $ rewrite a b
    unify' a (Var b) = unify' (Var b) a
    unify' (Merged a b) c = unify' a c <||> unify' b c -- XXX these two don't seem right
    unify' a (Merged b c) = unify' a b <||> unify' a c
    unify' a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

    unifyFE (FVar a) (FVar b) | a == b    = return Nothing
                              | otherwise = return . Just $ rewriteFE b (FVar a)
    unifyFE (FVar a) b = return . Just $ rewriteFE a b
    unifyFE a (FVar b) = unifyFE (FVar b) a
    unifyFE (FLit a) (FLit b) | a == b = return Nothing
    unifyFE (FOr a b) (FOr c d) = unifyFE a c <||> unifyFE b d
    unifyFE (FAffine   ty) (FAffine   ty') | ty == ty' = return Nothing     -- XXX what do I do here?
    unifyFE (FRelevant ty) (FRelevant ty') | ty == ty' = return Nothing
    unifyFE a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

unifyBlock :: Type -> Type -> [TypedOp] -> StateT TypeContext (Either String) TypedOp
unifyBlock s (Block aff rel a b :* s') ops = do
    ops' <- unifyTypes ops
    case ops' of
        [] -> return . Typed s (Block aff rel a b :* s') $ LitBlock []
        _  -> let Typed a' _  _ = head ops'
                  Typed _  b' _ = last ops'
              in return . Typed s (Block aff rel a' b' :* s') $ LitBlock ops'
unifyBlock _ _ _ = error "called unifyBlock with non-block type"

unifyOps :: [TypedOp] -> TypedOp -> StateT TypeContext (Either String) [TypedOp]
unifyOps [] (Typed l r (LitBlock ops')) = (:[]) <$> unifyBlock l r ops'
unifyOps [] op = return [op]
unifyOps (Typed tyll tylr opl:ops) (Typed tyrl tyrr (LitBlock ops')) = do
    op@(Typed l _ _) <- unifyBlock tyrl tyrr ops'
    x <- unify tylr l $ op:Typed tyll tylr opl:ops
    return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
unifyOps (Typed tyll tylr opl:ops) (Typed tyrl tyrr opr) = do
    x <- unify tylr tyrl $ Typed tyrl tyrr opr:Typed tyll tylr opl:ops
    return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
