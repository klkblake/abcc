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

derefTypes :: (Functor m, Monad m) => (Type -> Type -> StateT TypeContext m a) -> Type -> Type -> StateT TypeContext m a
derefTypes f (Var a) b = do
    a' <- deref a
    case a' of
        Just a'' -> derefTypes f a'' b
        Nothing ->
            case b of
                Var b' -> do
                    b'' <- deref b'
                    case b'' of
                        Just b''' -> derefTypes f (Var a) b'''
                        Nothing -> f (Var a) b
                _ -> f (Var a) b
derefTypes f a (Var b) = do
    b' <- deref b
    case b' of
        Just b'' -> derefTypes f a b''
        Nothing -> f a (Var b)
derefTypes f a b = f a b

derefTypesFE :: (Functor m, Monad m) => (FlagExpr -> FlagExpr -> StateT TypeContext m a) -> FlagExpr -> FlagExpr -> StateT TypeContext m a
derefTypesFE f (FVar a) b = do
    a' <- derefFE a
    case a' of
        Just a'' -> derefTypesFE f a'' b
        Nothing ->
            case b of
                FVar b' -> do
                    b'' <- derefFE b'
                    case b'' of
                        Just b''' -> derefTypesFE f (FVar a) b'''
                        Nothing -> f (FVar a) b
                _ -> f (FVar a) b
derefTypesFE f a (FVar b) = do
    b' <- derefFE b
    case b' of
        Just b'' -> derefTypesFE f a b''
        Nothing -> f a (FVar b)
derefTypesFE f a b = f a b

normalise :: (Functor m, Monad m) => Type -> Type -> StateT TypeContext m Type
normalise = derefTypes normalise'
  where
    normalise' a b | trace ("Normalise: " ++ show a ++ " <+> " ++ show b) False = undefined
    normalise' (a :*  b) (c :*  d) = (:*) <$> normalise a c <*> normalise b d
    normalise' (a :+  b) (c :+  d) = (:+) <$> normalise a c <*> normalise b d
    normalise' (Block aff rel a b) (Block aff' rel' c d) = Block <$> normaliseFE aff aff' <*> normaliseFE rel rel' <*> normalise a c <*> normalise b d
    normalise' Num Num = return Num
    normalise' Unit Unit = return Unit
    normalise' (Void a) (Void b) = Void <$> normalise a b
    normalise' (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA <$> normalise a b
    normalise' (Fix a b) (Fix c d) | a == c = Fix a <$> normalise b d
    normalise' (Var a) (Var b) | a == b = return (Var a)
    normalise' (Merged a b) (Merged c d) = do
        x <- normalise a b
        y <- normalise c d
        case (x, y) of
            (Merged _ _, _) -> return $ Merged x y
            (_, Merged _ _) -> return $ Merged x y
            _ -> normalise x y
    normalise' (Merged a b) c = do
        x <- normalise a b
        case x of
            Merged _ _ -> return $ Merged x c
            _ -> normalise x c
    normalise' a (Merged b c) = normalise (Merged b c) a
    normalise' (Var a) b = return $ Merged (Var a) b
    normalise' a (Var b) = return $ Merged a (Var b)
    normalise' a b = return $ Merged a b

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
referenced v (Merged a b) = (||) <$> referenced v a <*> referenced v b

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

canonicalFE :: String -> StateT TypeContext (Either String) String
canonicalFE var = do
    val <- derefFE var
    case val of
        Just (FVar var') -> canonicalFE var'
        _ -> return var

impose :: Type -> Type -> StateT TypeContext (Either String) Type
impose = derefTypes impose'
  where
    impose' (a :* b) (c :* d) = (:*) <$> impose a c <*> impose b d
    impose' (a :+ b) (c :+ d) = (:+) <$> impose a c <*> impose b d
    impose' (Block aff rel a b) (Block aff' rel' c d) = Block <$> imposeFE aff aff' <*> imposeFE rel rel' <*> impose a c <*> impose b d
    impose' Num Num = return Num
    impose' Unit Unit = return Unit
    impose' (Void a) (Void b) = Void <$> unify a b
    impose' (Sealed sealA a) (Sealed sealB b) | sealA == sealB = Sealed sealA <$> impose a b
                                              | otherwise = fail $ "Mismatched seals: " ++ show sealA ++ " /= " ++ show sealB
    impose' (Fix a b) (Fix c d) | a == c = Fix a <$> impose b d
    impose' (Var _) b = return b
    impose' (Merged _ _) (Merged _ _) = error "Don't know how to impose Merged"
    impose' (a :* b) (Var c) = do
        [a', b'] <- map Var <$> mapM fresh ["a", "b"]
        a'' <- impose a a'
        b'' <- impose b b'
        link c $ a'' :* b''
        return $ Var c
    impose' (a :+ b) (Var c) = do
        [a', b'] <- map Var <$> mapM fresh ["a", "b"]
        a'' <- impose a a'
        b'' <- impose b b'
        link c $ a'' :+ b''
        return $ Var c
    impose' (Block aff rel a b) (Var c) = do
        [aff', rel'] <- map FVar <$> mapM fresh ["aff", "rel"]
        [a', b'] <- map Var <$> mapM fresh ["a", "b"]
        aff'' <- imposeFE aff aff'
        rel'' <- imposeFE rel rel'
        a'' <- impose a a'
        b'' <- impose b b'
        link c $ Block aff'' rel'' a'' b''
        return $ Var c
    impose' Num (Var a) = link a Num >> return Num
    impose' Unit (Var a) = link a Unit >> return Unit
    impose' (Void a) (Var b) = do
        a' <- Var <$> fresh "a"
        a'' <- impose a a'
        link b $ Void a''
        return $ Var b
    impose' (Sealed sealA a) (Var b) = do
        a' <- Var <$> fresh "a"
        a'' <- impose a a'
        link b $ Sealed sealA a''
        return $ Var b
    impose' (Fix _ _) (Var _) = error "Don't know how to impose Fix"
    impose' (Merged _ _) (Var _) = error "Don't know how to impose Merged"
    impose' a (Merged b c) = Merged <$> impose a b <*> impose a c
    impose' a b = do
        strA <- showType a
        strB <- showType b
        error $ "Imposed invalid structure: " ++ strA ++ " on " ++ strB

imposeFE :: FlagExpr -> FlagExpr -> StateT TypeContext (Either String) FlagExpr
imposeFE = derefTypesFE imposeFE'
  where
    imposeFE' (FLit a) (FLit b) | a /= b = fail $ "Could not unify flag " ++ show a ++ " with " ++ show b
    imposeFE' _ b = return b

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
            refed <- referenced a b
            if refed -- XXX We actually need to check the entire chain.
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
        Merged l r -> do
            oldL <- reify l
            oldR <- reify r
            l' <- impose d l
            r' <- impose d r
            newL <- reify l'
            newR <- reify r'
            strAI <- showType $ Merged l' r'
            if trace (strA' ++ " imposed to " ++ strAI) $ oldL /= newL || oldR /= newR
                then unify (Merged l' r') d
                else do
                    strA'' <- showType (Merged l' r')
                    strB <- showType d
                    error $ "Don't know how to unify " ++ strA'' ++ " with " ++ strB
        _ -> unify a' d
unify a b@(Merged _ _) = do
    unify b a
    --strA <- showType a
    --strB <- showType b
    --error $ "Don't know how to unify " ++ strA ++ " with " ++ strB
unify a b = do
    strA <- showType a
    strB <- showType b
    fail $ "Could not unify " ++ strA ++ " with " ++ strB

unifyFE :: FlagExpr -> FlagExpr -> StateT TypeContext (Either String) FlagExpr
unifyFE (FVar a) (FVar b) | a == b    = return $ FVar a
                          | otherwise = do
                              a' <- canonicalFE a
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
    --return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
    return x
unifyOps (Typed tyll tylr opl:ops) (Typed tyrl tyrr opr) = do
    ty <- unify tylr tyrl
    let x = Typed ty tyrr opr:Typed tyll ty opl:ops
    --return $ trace ("Current type:\n" ++ intercalate "\n" (map show x)) x
    return x

unifyTypes :: [TypedOp] -> StateT TypeContext (Either String) [TypedOp]
unifyTypes ops = reverse <$> foldM unifyOps [] ops
