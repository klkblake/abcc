module ResolveFlags where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Type
import Op

fixedM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixedM f x = do
    x' <- f x
    if x == x' then return x else fixedM f x'

deref' :: (Monad m, Functor m) => String -> (Type -> StateT TypeContext m a) -> StateT TypeContext m a -> StateT TypeContext m a
deref' v f a = do
    v' <- deref v
    case v' of
        Just v'' -> f v''
        Nothing -> a

resolve :: (Monad m, Functor m) => Type -> StateT TypeContext m Type
resolve (a :* b) = (:*) <$> resolve a <*> resolve b
resolve (a :+ b) = (:+) <$> resolve a <*> resolve b
resolve (Block aff rel a b) = Block <$> resolveFE aff <*> resolveFE rel <*> resolve a <*> resolve b
resolve Num = return Num
resolve Unit = return Unit
resolve (Void a) = Void <$> resolve a
resolve (Sealed s a) = Sealed s <$> resolve a
resolve (Fix a b) = Fix a <$> resolve b
resolve (Var a) = return $ Var a

resolveFE :: (Monad m, Functor m) => FlagExpr -> StateT TypeContext m FlagExpr
resolveFE (FVar a) = do
    a' <- derefFE a
    case a' of
        Just a'' -> resolveFE a''
        Nothing  -> return $ FVar a
resolveFE (FOr a b) = do
    a' <- resolveFE a
    b' <- resolveFE b
    return $ case a' of
                 FLit True  -> FLit True
                 FLit False -> b'
                 _ -> case b' of
                          FLit True -> FLit True
                          FLit False -> a'
                          _ -> FOr a' b'
resolveFE (FAffine   (Block aff _ _ _)) = resolveFE aff
resolveFE (FRelevant (Block _ rel _ _)) = resolveFE rel
resolveFE (FAffine   (Var a)) = deref' a (resolveFE . FAffine)   . return . FAffine   $ Var a
resolveFE (FRelevant (Var a)) = deref' a (resolveFE . FRelevant) . return . FRelevant $ Var a
resolveFE (FAffine   ty) = FAffine   <$> resolve ty
resolveFE (FRelevant ty) = FRelevant <$> resolve ty
resolveFE fe = return fe

resolveFlags :: (Monad m, Functor m) => [PTypedOp] -> StateT TypeContext m [PTypedOp]
resolveFlags ops = do
    fes <- gets tcx_subgraphs_fe
    _ <- fixedM resolveFlags' fes
    tys <- gets tcx_subgraphs
    tys' <- mapM (onSnd resolve) $ Map.toList tys
    modifySubgraphs . const $ Map.fromList tys'
    modifySubgraphsFE $ const Map.empty
    return ops
  where
    resolveFlags' fes = do
        fes' <- mapM (onSnd resolveFE) $ Map.toList fes
        modifySubgraphsFE . const $ Map.fromList fes'
        gets tcx_subgraphs_fe
    onSnd f (a, b) = do
        b' <- f b
        return (a, b')
