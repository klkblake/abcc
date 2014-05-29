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
normalise (a :~> b) (c :~> d) = normalise a c ~> normalise b d
normalise Num Num = Num
normalise Unit Unit = Unit
normalise (Void a) (Void b) = Void $ normalise a b
normalise (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA $ normalise a b
normalise (Fix a b) (Fix c d) | a == c = Fix a $ normalise b d
normalise (Var a) (Var b) | a == b = Var a
normalise (Merged a b) (Merged c d) = let x = normalise a b
                                          y = normalise c d
                                      in case (x, y) of
                                             (Merged _ _, _) -> Merged x y
                                             (_, Merged _ _) -> Merged x y
                                             _ -> normalise x y
normalise a b = Merged a b

mapTy :: (Type -> Maybe Type) -> Type -> Type
mapTy f ty | Just ty' <- f ty = ty'
mapTy f (a :*  b)       = mapTy f a :*  mapTy f b
mapTy f (a :+  b)       = mapTy f a :+  mapTy f b
mapTy f (a :~> b)       = mapTy f a :~> mapTy f b
mapTy _ Num             = Num
mapTy _ Unit            = Unit
mapTy f (Void a)        = Void $ mapTy f a
mapTy f (Sealed seal a) = Sealed seal $ mapTy f a
mapTy f (Fix v a)       = Fix v $ mapTy f a
mapTy _ (Var a)         = Var a
mapTy f (Merged a b)    = normalise (mapTy f a) (mapTy f b)

rewrite :: String -> Type -> Type -> Type
rewrite v ty = mapTy rewrite'
  where
    rewrite' (Fix v' a) | v == v', Var b <- ty = Just . Fix b $ rewrite v ty a
    rewrite' (Fix v' _) | v == v' = error "Attempted to rewrite var in Fix"
    rewrite' (Var a) | v == a = Just ty
    rewrite' _ = Nothing

roll :: Type -> String -> Type -> Type
roll ty v = mapTy roll'
  where
    roll' ty' | ty == ty' = Just $ Var v
    roll' _ = Nothing

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
    referenced v (Fix _ a) = referenced v a
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
    unify' (Merged a b) c = unify' a c <||> unify' b c
    unify' a (Merged b c) = unify' a b <||> unify' a c
    unify' a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

unifyOps :: [TypedOp] -> TypedOp -> StateT TypeContext (Either String) [TypedOp]
unifyOps [] op = return [op]
unifyOps (Typed tyl@(_ :~> tylr) opl:ops) (Typed (s :~> (a :~> b) :* s') (LitBlock ops')) = do
    ops'' <- unifyTypes ops'
    case ops'' of
        [] -> unify tylr s $ Typed (s ~> (a ~> b) :* s') (LitBlock []):Typed tyl opl:ops
        _  -> do
            let Typed (a' :~> _)  _ = head ops''
            let Typed (_  :~> b') _ = last ops''
            x <- unify tylr s $ Typed (s ~> (a' ~> b') :* s') (LitBlock ops''):Typed tyl opl:ops
            return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
unifyOps (Typed tyl@(_ :~> tylr) opl:ops) (Typed tyr@(tyrl :~> _) opr) = do x <- unify tylr tyrl $ Typed tyr opr:Typed tyl opl:ops
                                                                            return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
unifyOps _ _ = error "called unifyOps on list containing op with non-block type"

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
