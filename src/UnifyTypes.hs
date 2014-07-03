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

normalise :: (Functor m, Monad m) => Type -> Type -> StateT TypeContext m Type
normalise a b | trace ("Normalise: " ++ show a ++ " <+> " ++ show b) False = undefined
normalise (a :*  b) (c :*  d) = (:*) <$> normalise a c <*> normalise b d
normalise (a :+  b) (c :+  d) = (:+) <$> normalise a c <*> normalise b d
normalise (Block aff rel a b) (Block aff' rel' c d) = Block <$> normaliseFE aff aff' <*> normaliseFE rel rel' <*> normalise a c <*> normalise b d
normalise Num Num = return Num
normalise Unit Unit = return Unit
normalise (Void a) (Void b) = Void <$> normalise a b
normalise (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA <$> normalise a b
normalise (Fix a b) (Fix c d) | a == c = Fix a <$> normalise b d
normalise (Var a) b = do
    a' <- deref a
    case a' of
        Just a'' -> normalise a'' b
        Nothing ->
            case b of
                Var b' -> do
                    b'' <- deref b'
                    case b'' of
                        Just b''' -> normalise (Var a) b'''
                        Nothing | a == b'   -> return $ Var a
                                | otherwise -> return $ Merged (Var a) b
                _ -> return $ Merged (Var a) b
normalise a (Var b) = normalise (Var b) a
normalise (Merged a b) (Merged c d) = do
    x <- normalise a b
    y <- normalise c d
    case (x, y) of
        (Merged _ _, _) -> return $ Merged x y
        (_, Merged _ _) -> return $ Merged x y
        _ -> normalise x y
normalise a b = return $ Merged a b

normaliseFE :: (Functor m, Monad m) => FlagExpr -> FlagExpr -> StateT TypeContext m FlagExpr
normaliseFE (FVar a) b = do
    a' <- derefFE a
    case a' of
        Just a'' -> normaliseFE a'' b
        Nothing ->
            case b of
                FVar b' -> do
                     b'' <- derefFE b'
                     case b'' of
                         Just b''' -> normaliseFE (FVar a) b'''
                         Nothing | a == b'   -> return $ FVar a
                                 | otherwise -> return $ FOr (FVar a) b
                _ -> return $ FOr (FVar a) b
normaliseFE (FLit a) (FLit b) = return . FLit $ a || b
normaliseFE (FOr a b) (FOr c d) = do
    x <- normaliseFE a b
    y <- normaliseFE c d
    case (x, y) of
        (FOr _ _, _) -> return $ FOr x y
        (_, FOr _ _) -> return $ FOr x y
        _ -> normaliseFE x y
normaliseFE (FAffine   ty) (FAffine   ty') = FAffine   <$> normalise ty ty'
normaliseFE (FRelevant ty) (FRelevant ty') = FRelevant <$> normalise ty ty'
normaliseFE a b = return $ FOr a b

{-
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
-}

referenced :: String -> Type -> Bool
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

referencedFE :: String -> FlagExpr -> Bool
referencedFE v (FVar var) = v == var
referencedFE _ (FLit _)   = False
referencedFE v (FOr  a b) = referencedFE v a || referencedFE v b
referencedFE v (FAffine   ty) = referenced v ty
referencedFE v (FRelevant ty) = referenced v ty

(<||>) :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
a <||> b = do
    a' <- a
    b' <- b
    return $ a' <|> b'

canonical :: String -> StateT TypeContext (Either String) String
canonical var = do
    val <- deref var
    case val of
        Just (Var var') -> canonical var'
        _ -> return var

unify :: Type -> Type -> StateT TypeContext (Either String) Type
unify (a :*  b) (c :*  d) = (:*) <$> unify a c <*> unify b d
unify (a :+  b) (c :+  d) = (:+) <$> unify a c <*> unify b d
unify (Block aff rel a b) (Block aff' rel' c d) = Block <$> unifyFE aff aff' <*> unifyFE rel rel' <*> unify a c <*> unify b d
unify Num       Num       = return Num
unify Unit      Unit      = return Unit
unify (Void a)  (Void b)  = Void <$> unify a b
unify (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA <$> unify a b
                                        | otherwise = fail $ "Mismatched seals: " ++ show sealA ++ " /= " ++ show sealB
unify (Fix a b) (Fix c d) | a == c = Fix a <$> unify b d
unify (Var a) (Var b) | a == b    = return $ Var a
                      | otherwise = do
    a' <- deref a
    case a' of
        Just a'' -> unify a'' (Var b)
        Nothing -> do
            b' <- deref b
            case b' of
                Just b'' -> unify (Var a) b''
                Nothing -> do
                    cs <- Map.findWithDefault [] b <$> gets tcx_constraints
                    modifyConstraints $ Map.delete b
                    mapM_ (constrain a) $ trace ("rewrite " ++ show a ++ " " ++ show b) cs
                    link b (Var a)
                    return $ Var a
unify (Var a) b = do
    a' <- deref a
    case a' of
        Just a'' -> unify a'' b
        Nothing -> do
            strB <- showType b
            if referenced a b -- XXX We actually need to check the entire chain.
                --then trace ("fix " ++ show a ++ " " ++ show b) $ return . Just $ rewrite a (Fix a b) . fixed (roll b a)
                then trace ("fix " ++ show a ++ " " ++ strB) $ do
                    propagateConstraints a b
                    return $ Fix a b -- Can't use a' here without changing references
                else do
                    link a b
                    trace ("rewrite " ++ show a ++ " " ++ strB) . return $ Var a
unify a (Var b) = do
    b' <- deref b
    case b' of
        Just b'' -> unify a b''
        Nothing -> unify (Var b) a
unify (Merged a b) (Merged c d) | a == c && b == d = return $ Merged a c
unify a@(Merged b c) d = do
    a' <- normalise b c
    strA <- showType a
    strA' <- showType a'
    case trace (strA ++ " normalised to " ++ strA') a' of
        Merged _ _ -> do
            strA <- showType a
            strB <- showType d
            error $ "Don't know how to unify " ++ strA ++ " with " ++ strB
        _ -> unify a' d
unify a b@(Merged _ _) = do
    strA <- showType a
    strB <- showType b
    error $ "Don't know how to unify " ++ strA ++ " with " ++ strB
unify a b = do
    strA <- showType a
    strB <- showType b
    fail $ "Could not unify " ++ strA ++ " with " ++ strB

unifyFE :: FlagExpr -> FlagExpr -> StateT TypeContext (Either String) FlagExpr
unifyFE (FVar a) (FVar b) | a == b    = return $ FVar a
                          | otherwise = do
                              a' <- canonical a
                              linkFE b $ FVar a'
                              return $ FVar a'
unifyFE (FVar a) b = linkFE a b >> return (FVar a)
unifyFE a (FVar b) = unifyFE (FVar b) a
unifyFE (FLit a) (FLit b) | a == b = return $ FLit a
unifyFE (FOr a b) (FOr c d) = FOr <$> unifyFE a c <*> unifyFE b d
unifyFE (FAffine   ty) (FAffine   ty') | ty == ty' = return $ FAffine ty     -- XXX what do I do here?
unifyFE (FRelevant ty) (FRelevant ty') | ty == ty' = return $ FRelevant ty
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
    (Typed tyrl' tyrr' opr) <- unifyBlock tyrl tyrr ops'
    ty <- unify tylr tyrl'
    let x = Typed ty tyrr' opr:Typed tyll ty opl:ops
    return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
unifyOps (Typed tyll tylr opl:ops) (Typed tyrl tyrr opr) = do
    ty <- unify tylr tyrl
    let x = Typed ty tyrr opr:Typed tyll ty opl:ops
    return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
