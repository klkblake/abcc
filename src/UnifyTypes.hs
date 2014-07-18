{-# LANGUAGE MultiWayIf #-}
module UnifyTypes (unifyTypes) where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Type
import Op

derefTypes :: (Functor m, Monad m) => (Type -> Type -> StateT TypeContext m a) -> Type -> Type -> StateT TypeContext m a
derefTypes f (Var a) b = do
    deref' a (flip (derefTypes f) b) $
        case b of
            Var b' -> deref' b' (derefTypes f $ Var a) $ f (Var a) b
            _      -> f (Var a) b
derefTypes f a (Var b) = deref' b (derefTypes f a) $ f a (Var b)
derefTypes f a b = f a b

derefTypesFE :: (Functor m, Monad m) => (FlagExpr -> FlagExpr -> StateT TypeContext m a) -> FlagExpr -> FlagExpr -> StateT TypeContext m a
derefTypesFE f (FVar a) b = do
    derefFE' a (flip (derefTypesFE f) b) $
        case b of
            FVar b' -> derefFE' b' (derefTypesFE f $ FVar a) $ f (FVar a) b
            _       -> f (FVar a) b
derefTypesFE f a (FVar b) = derefFE' b (derefTypesFE f a) $ f a (FVar b)
derefTypesFE f a b = f a b

referenced :: (Functor m, Monad m) => String -> Type -> StateT TypeContext m Bool
referenced v (a :*  b) = (||) <$> referenced v a <*> referenced v b
referenced v (a :+  b) = (||) <$> referenced v a <*> referenced v b
referenced v (Block aff rel a b) = or <$> sequence [referencedFE v aff, referencedFE v rel, referenced v a, referenced v b]
referenced _ Num = return False
referenced _ Unit = return False
referenced v (Void a) = referenced v a
referenced v (Sealed _ a) = referenced v a
referenced v (Fix _ a) = referenced v a
referenced v (Var a) = do
    a' <- deref a
    case a' of
        Just a'' -> referenced v a''
        Nothing -> return $ v == a

referencedFE :: (Functor m, Monad m) => String -> FlagExpr -> StateT TypeContext m Bool
referencedFE v (FVar var) = do
    var' <- derefFE var
    case var' of
        Just var'' -> referencedFE v var''
        Nothing -> return $ v == var
referencedFE _ (FLit _)   = return False
referencedFE v (FOr  a b) = (||) <$> referencedFE v a <*> referencedFE v b
referencedFE v (FAffine   ty) = referenced v ty
referencedFE v (FRelevant ty) = referenced v ty

unify :: Type -> Type -> StateT TypeContext (Either String) Type
unify = derefTypes unify'
  where
    unify' (a :*  b) (c :*  d) = (:*) <$> unify a c <*> unify b d
    unify' (a :+  b) (c :+  d) = (:+) <$> unify a c <*> unify b d
    unify' (Block aff rel a b) (Block aff' rel' c d) = Block <$> unifyFE aff aff' <*> unifyFE rel rel' <*> unify a c <*> unify b d
    unify' Num       Num       = return Num
    unify' Unit      Unit      = return Unit
    unify' (Void a)  (Void b)  = Void <$> unify a b
    unify' (Void a)  b         = Void <$> unify a b
    unify' a         (Void b)  = Void <$> unify a b
    unify' (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA <$> unify a b
                                             | otherwise = fail $ "Mismatched seals: " ++ show sealA ++ " /= " ++ show sealB
    unify' (Fix a b) (Fix c d) | a == c = Fix a <$> unify b d
    unify' (Var a) (Var b) | a == b    = return $ Var a
                           | otherwise = do
                               cs <- Map.findWithDefault [] b <$> gets tcx_constraints
                               modifyConstraints $ Map.delete b
                               mapM_ (constrain a) cs
                               link b $ Var a
                               return $ Var a
    unify' (Var a) b = do
        refed <- referenced a b
        if refed -- XXX We actually need to check the entire chain.
            then propagateConstraints a b >> return (Fix a b) -- Can't use a' here without changing references
            else link a b >> return (Var a)
    unify' a (Var b) = unify (Var b) a
    unify' a b = do
        strA <- showType a
        strB <- showType b
        lift $ Left $ "Could not unify " ++ strA ++ " with " ++ strB

unifyFE :: FlagExpr -> FlagExpr -> StateT TypeContext (Either String) FlagExpr
unifyFE = derefTypesFE unifyFE'
  where
    unifyFE' (FVar a) (FVar b) | a == b    = return $ FVar a
                               | otherwise = do
                                   linkFE b $ FVar a
                                   return $ FVar a
    unifyFE' (FVar a) b = linkFE a b >> return (FVar a)
    unifyFE' a (FVar b) = unifyFE (FVar b) a
    unifyFE' (FLit a) (FLit b) | a == b = return $ FLit a
    unifyFE' (FOr a b) (FOr c d) = FOr <$> unifyFE a c <*> unifyFE b d
    unifyFE' (FAffine   ty) (FAffine   ty') | ty == ty' = return $ FAffine ty     -- XXX what do I do here?
    unifyFE' (FRelevant ty) (FRelevant ty') | ty == ty' = return $ FRelevant ty
    unifyFE' a b = fail $ "Could not unify " ++ show a ++ " with " ++ show b

unifyBlock :: String -> String -> [PTypedOp] -> StateT TypeContext (Either String) PTypedOp
unifyBlock s bl ops = do
    ops' <- unifyTypes ops
    Just (Block aff rel _ _ :* s') <- deref bl
    case ops' of
        [] -> return . PartiallyTyped s bl $ LitBlock []
        _  -> do
            let PartiallyTyped a' _  _ = head ops'
                PartiallyTyped _  b' _ = last ops'
            bl' <- fresh bl
            link bl' $ Block aff rel (Var a') (Var b') :* s'
            return . PartiallyTyped s bl' $ LitBlock ops'

unifyOps' :: [PTypedOp] -> PTypedOp -> StateT TypeContext (Either String) [PTypedOp]
unifyOps' ops'@(PartiallyTyped tyll tylr opl:ops) op@(PartiallyTyped tyrl tyrr opr) = do
    tcx <- get
    ty <- mapStateT (errorContext tcx) $ unify (Var tylr) (Var tyrl)
    v <- fresh "op"
    link v ty
    return $ PartiallyTyped v tyrr opr:PartiallyTyped tyll v opl:ops
  where
    errorContext tcx (Left err) = flip evalStateT tcx $ do
        ops'' <- mapM reifyPTyped ops'
        (Typed l r op') <- reifyPTyped op
        lift $ Left $ err ++ "\n\twhile unifying\n" ++ show op' ++ ":\t" ++ show l ++ " -> " ++ show r ++ "\n\twith\n" ++ showOps 0 (reverse ops'')
    errorContext _ (Right val) = Right val
unifyOps' [] op = return [op]

unifyOps :: [PTypedOp] -> PTypedOp -> StateT TypeContext (Either String) [PTypedOp]
unifyOps [] (PartiallyTyped l r (LitBlock ops')) = (:[]) <$> unifyBlock l r ops'
unifyOps (PartiallyTyped tyll tylr opl:ops) (PartiallyTyped tyrl tyrr (LitBlock ops')) = do
    PartiallyTyped tyrl' tyrr' opr <- unifyBlock tyrl tyrr ops'
    unifyOps' (PartiallyTyped tyll tylr opl:ops) (PartiallyTyped tyrl' tyrr' opr)
unifyOps ops opr = unifyOps' ops opr

unifyTypes :: [PTypedOp] -> StateT TypeContext (Either String) [PTypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
