{-# LANGUAGE LambdaCase #-}
module ResolveTypes where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map.Strict as Map

import Type
import Op

import UnifyTypes (normalise)

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

elimVars :: (Monad m, Functor m) => Type -> Type -> StateT TypeContext m Type
--elimVars (Var a) b = deref' a (flip elimVars b) $ return Unit
--elimVars a (Var b) = deref' b      (elimVars a) $ return Unit
--elimVars (Merged a b) c = elimVars a b >>= flip elimVars c
--elimVars a (Merged b c) = elimVars b c >>= elimVars a
elimVars a b = return $ Merged a b

resolve :: (Monad m, Functor m) => Type -> StateT TypeContext m Type
resolve (a :* b) = (:*) <$> resolve a <*> resolve b
resolve (a :+ b) = (:+) <$> resolve a <*> resolve b
resolve (Block aff rel a b) = Block <$> return aff <*> return rel <*> resolve a <*> resolve b
resolve Num = return Num
resolve Unit = return Unit
resolve (Void a) = Void <$> resolve a
resolve (Sealed s a) = Sealed s <$> resolve a
resolve (Fix a b) = Fix a <$> resolve b
--resolve (Var a) = deref' a resolve . return $ Var a
resolve (Var a) = return $ Var a
resolve (Merged a b) = do
    a' <- resolve a
    b' <- resolve b
    ty <- flip fixedM (Merged a' b') $ \case
                                            Merged a'' b'' -> do
                                                ty <- elimVars a'' b''
                                                case ty of
                                                    Merged a3 b3 -> normalise a3 b3
                                                    _ -> return ty
                                            ty -> return ty
    case ty of
        Merged _ _ -> return ty
        _ -> resolve ty


resolveTypes :: (Monad m, Functor m) => [PTypedOp] -> StateT TypeContext m [PTypedOp]
resolveTypes ops = do
    tys <- gets tcx_subgraphs
    _ <- fixedM resolveTypes' tys
    return ops
  where
    resolveTypes' tys = do
        tys' <- mapM (onSnd resolve) $ Map.toList tys
        modifySubgraphs . const $ Map.fromList tys'
        gets tcx_subgraphs
    onSnd f (a, b) = do
        b' <- f b
        return (a, b')
