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

renameType :: Maybe (String, Constraint) -> Type -> State TypeContext Type
renameType cs ty = do
    (ty', (_, cs')) <- runStateT (renameType' ty) (Map.empty, cs)
    case cs' of
        Nothing -> return ()
        Just (v, c') -> constrain v c'
    return ty'
  where
    renameType' :: Type -> StateT (Map.Map String String, Maybe (String, Constraint)) (State TypeContext) Type
    renameType' (l :* r)          = (:*) <$> renameType' l <*> renameType' r
    renameType' (l :+ r)          = (:+) <$> renameType' l <*> renameType' r
    renameType' (l :~> r)         = (~>) <$> renameType' l <*> renameType' r
    renameType' Num               = return Num
    renameType' Unit              = return Unit
    renameType' (Void ty')        = Void        <$> renameType' ty'
    renameType' (Sealed seal ty') = Sealed seal <$> renameType' ty'
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

addE :: Type -> Type
addE (l :~> r) = (addE' l) ~> (addE' r)
  where
    addE' (l' :* r') = l' :* (addE' r')
    addE' ty = ty :* e
addE _ = error "called addE on non-block type"

#define OP(op, ty) typedOp (op) = (flip Typed (op)) <$> (renameType Nothing $ ty)
#define OPe(op, ty) typedOp (op) = (flip Typed (op)) <$> (renameType Nothing . addE $ ty)
#define OPc(op, ty, v, c) typedOp (op) = (flip Typed (op)) <$> (renameType (Just (v, c)) $ ty)
#define OPce(op, ty, v, c) typedOp (op) = (flip Typed (op)) <$> (renameType (Just (v, c)) . addE $ ty)

typedOp :: UntypedOp -> State TypeContext TypedOp
typedOp (LitBlock block) = do
    ops <- mapM (typedOp . runIdentity) block
    case ops of
        [] -> flip Typed (LitBlock []) <$> renameType Nothing (s ~> (a ~> a) :* s)
        _  -> do
            let Typed (a' :~> _)  _ = head ops
            let Typed (_  :~> b') _ = last ops
            flip Typed (LitBlock ops) <$> renameType Nothing (s ~> (a' ~> b') :* s)
OP(LitText text, s ~> Merged (Var "L") (x :* Var "L" :+ Unit) :* s)

OP(AssocL, a :* b :* c ~> (a :* b) :* c)
OP(AssocR, (a :* b) :* c ~> a :* b :* c)
OP(Swap, a :* b :* c ~> b :* a :* c)
OP(SwapD, a :* b :* c :* d ~> a :* c :* b :* d)
OP(Intro1, a ~> a :* Unit)
OP(Elim1, a :* Unit ~> a)
OPc(Drop, x :* e ~> e, "x", Droppable)
OPce(Copy, x ~> x :* x, "x", Copyable)

OPe(Apply, (x ~> xp) :* x ~> xp)
-- XXX This probably should only appear later in the pipeline
OP(ApplyTail, (x ~> xp) :* x :* Unit ~> xp)
OPe(Compose, (y ~> z) :* (x ~> y) ~> (x ~> z))
OPce(Quote, x ~> (s ~> x :* s), "x", Quotable)
OPe(Relevant, (x ~> y) ~> (x ~> y))
OPe(Affine, (x ~> y) ~> (x ~> y))

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

OPce(CondApply, Merged b (x ~> xp) :* (x :+ y) ~> xp :+ y, "b", Droppable)
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
