{-# LANGUAGE CPP #-}
module AddTypes where

import Control.Applicative
import Control.Monad.State hiding (ap)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

import Type
import Op

a, ap, b, c, d, e, s, x, xp, y, z :: Type
a = Var "a"
ap = Var "a'"
b = Var "b"
c = Var "c"
d = Var "d"
e = Var "e"
s = Var "s"
x = Var "x"
xp = Var "x'"
y = Var "y"
z = Var "z"

aff, rel, fa, fr, ga, gr :: FlagExpr
aff = FVar "aff"
rel = FVar "rel"
fa = FVar "fa"
fr = FVar "fr"
ga = FVar "ga"
gr = FVar "gr"

(~>) :: Type -> Type -> Type
(~>) = Block err err
  where
    err = error "Attempted to inspect affine/relevant attributes of an op"

infixr 1 ~>

renameType :: Maybe (String, Constraint) -> Type -> State TypeContext Type
renameType cs (Block _ _ tyl tyr) = do
    (tyl' :* tyr', (_, cs')) <- runStateT (renameType' (tyl :* tyr)) (Map.empty, cs)
    case cs' of
        Nothing -> return ()
        Just (v, c') -> constrain v c'
    return $ tyl' ~> tyr'
  where
    renameType' :: Type -> StateT (Map.Map String String, Maybe (String, Constraint)) (State TypeContext) Type
    renameType' (l :* r)              = (:*) <$> renameType' l <*> renameType' r
    renameType' (l :+ r)              = (:+) <$> renameType' l <*> renameType' r
    renameType' (Block aff' rel' l r) = Block <$> renameFE aff' <*> renameFE rel' <*> renameType' l <*> renameType' r
    renameType' Num                   = return Num
    renameType' Unit                  = return Unit
    renameType' (Void ty')            = Void        <$> renameType' ty'
    renameType' (Sealed seal ty')     = Sealed seal <$> renameType' ty'
    renameType' (Fix var ty') = do
        Var var' <- renameType' $ Var var
        Fix var' <$> renameType' ty'
    renameType' (Var var) = do
        (used, cs') <- get
        case Map.lookup var used of
            Just var' -> return $ Var var'
            Nothing -> do
                var' <- lift $ fresh var
                let cs'' = case cs' of
                               Just (v, c') | v == var -> Just (var', c')
                               _ -> cs'
                put $ (Map.insert var var' used, cs'')
                return $ Var var'
    renameType' (Merged l r) = Merged <$> renameType' l <*> renameType' r

    renameFE (FVar var) = do
        (used, cs') <- get
        case Map.lookup var used of
            Just var' -> return $ FVar var'
            Nothing -> do
                var' <- lift $ fresh var
                put $ (Map.insert var var' used, cs')
                return $ FVar var'
    renameFE (FLit v) = return $ FLit v
    renameFE (FOr l r)  = FOr <$> renameFE l <*> renameFE r
    renameFE (FAffine ty') = FAffine <$> renameType' ty'
    renameFE (FRelevant ty') = FRelevant <$> renameType' ty'
renameType _ _ = error "called renameType on non-block type"

addE :: Type -> Type
addE (Block aff' rel' l r) = Block aff' rel' (addE' l) (addE' r)
  where
    addE' (l' :* r') = l' :* (addE' r')
    addE' ty = ty :* e
addE _ = error "called addE on non-block type"

typed :: Op Typed -> Type -> TypedOp
typed op ty = let Block _ _ l r = ty
              in Typed l r op

#define OP(op, ty)         typedOp (op) = typed (op) <$> (renameType Nothing $ ty)
#define OPe(op, ty)        typedOp (op) = typed (op) <$> (renameType Nothing . addE $ ty)
#define OPc(op, ty, v, c)  typedOp (op) = typed (op) <$> (renameType (Just (v, c)) $ ty)
#define OPce(op, ty, v, c) typedOp (op) = typed (op) <$> (renameType (Just (v, c)) . addE $ ty)

typedOp :: UntypedOp -> State TypeContext TypedOp
typedOp (LitBlock block) = do
    ops <- mapM (typedOp . runIdentity) block
    case ops of
        [] -> typed (LitBlock []) <$> renameType Nothing (s ~> (Block (FLit False) (FLit False) a a) :* s)
        _  -> do
            let Typed a' _  _ = head ops
            let Typed _  b' _ = last ops
            typed (LitBlock ops) <$> renameType Nothing (s ~> (Block (FLit False) (FLit False) a' b') :* s)
OP(LitText text, s ~> Fix "L" (x :* Var "L" :+ Unit) :* s)

OP(AssocL, a :* b :* c ~> (a :* b) :* c)
OP(AssocR, (a :* b) :* c ~> a :* b :* c)
OP(Swap, a :* b :* c ~> b :* a :* c)
OP(SwapD, a :* b :* c :* d ~> a :* c :* b :* d)
OP(Intro1, a ~> a :* Unit)
OP(Elim1, a :* Unit ~> a)
OPc(Drop, x :* e ~> e, "x", Droppable)
OPce(Copy, x ~> x :* x, "x", Copyable)

OPe(Apply, (Block aff rel x xp) :* x ~> xp)
-- XXX This probably should only appear later in the pipeline
OP(ApplyTail, (Block aff rel x xp) :* x :* Unit ~> xp)
OPe(Compose, (Block ga gr y z) :* (Block fa fr x y) ~> (Block (FOr fa ga) (FOr fr gr) x z))
OPce(Quote, x ~> (Block (FAffine x) (FRelevant x) s $ x :* s), "x", Quotable)
OPe(Relevant, (Block aff rel x y) ~> (Block aff (FLit True) x y))
OPe(Affine, (Block aff rel x y) ~> (Block (FLit True) rel x y))

OP(IntroNum, e ~> Num :* e)
OPe(Digit digit, Num ~> Num)

OPe(Add, Num :* Num ~> Num)
OPe(Multiply, Num :* Num ~> Num)
OPe(Inverse, Num ~> Num)
OPe(Negate, Num ~> Num)
OPe(Divmod, Num :* Num ~> Num :* Num)

OPe(AssocLS, a :+ b :+ c ~> (a :+ b) :+ c)
OPe(AssocRS, (a :+ b) :+ c ~> a :+ b :+ c)
OPe(SwapS, a :+ b :+ c ~> b :+ a :+ c)
OPe(SwapDS, a :+ b :+ c :+ d ~> a :+ c :+ b :+ d)
OPe(Intro0, a ~> a :+ Void x)
OPe(Elim0, a :+ Void x ~> a)

OPe(CondApply, (Block aff (FLit False) x xp) :* (x :+ y) ~> xp :+ y)
OPe(Distrib, a :* (b :+ c) ~> a :* b :+ a :* c)
OPe(Factor, a :* b :+ c :* d ~> (a :+ c) :* (b :+ d))
OPe(Merge, a :+ ap ~> Merged a ap)
OPe(Assert, a :+ b ~> b)

OPe(Greater, Num :* Num ~> Num :* Num :+ Num :* Num)

OP(Sealer seal, a ~> Sealed seal a)
OP(Unsealer seal, Sealed seal a ~> a)

OPe(AssertEQ, a :* b ~> a :* b)
OP(DebugPrintRaw, error "DebugPrintRaw not supported. I need to implement the standard debug print system")
OP(DebugPrintText, error "DebugPrintText not supported. I need to implement the standard debug print system")

#undef OP

addTypes :: [UntypedOp] -> State TypeContext [TypedOp]
addTypes ops = mapM typedOp ops
