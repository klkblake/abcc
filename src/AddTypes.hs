{-# LANGUAGE CPP #-}
module AddTypes where

import Control.Applicative
import Control.Monad.State hiding (ap)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

import Type
import Op

a, ap, b, c, d, e, s, x, xp, y, z :: Type
a = Var [] "a"
ap = Var [] "a'"
b = Var [] "b"
c = Var [] "c"
d = Var [] "d"
e = Var [] "e"
s = Var [] "s"
x = Var [] "x"
xp = Var []"x'"
y = Var [] "y"
z = Var [] "z"

renameType :: Type -> State TypeContext Type
renameType ty = evalStateT (renameType' ty) Map.empty
  where
    renameType' (l :* r)         = (:*)     <$> renameType' l <*> renameType' r
    renameType' (l :+ r)         = (:+)     <$> renameType' l <*> renameType' r
    renameType' (Block bt l r)   = Block bt <$> renameType' l <*> renameType' r
    renameType' Num              = return Num
    renameType' Unit             = return Unit
    renameType' (Void ty')        = Void        <$> renameType' ty'
    renameType' (Sealed seal ty') = Sealed seal <$> renameType' ty'
    renameType' (Fix var ty') = do
        used <- get
        var' <- lift $ fresh var
        modify $ Map.insert var var'
        ty'' <- renameType' ty'
        put used
        return $ Fix var' ty''
    renameType' (Var cs var) = do
        used <- get
        case Map.lookup var used of
            Just var' -> return $ Var cs var'
            Nothing -> do
                var' <- lift $ fresh var
                modify $ Map.insert var var'
                return $ Var cs var'
    renameType' (Merged l r) = Merged <$> renameType' l <*> renameType' r

addE :: Type -> Type
addE (Block bt l r) = Block bt (addE' l) (addE' r)
  where
    addE' (l' :* r') = l' :* (addE' r')
    addE' ty = ty :* e
addE _ = error "called addE on non-block type"

#define OP(op, ty) typedOp (op) = (flip Typed (op)) <$> (renameType $ ty)
#define OPe(op, ty) typedOp (op) = (flip Typed (op)) <$> (renameType . addE $ ty)

typedOp :: UntypedOp -> State TypeContext TypedOp
typedOp (LitBlock block) = Typed (s ~> (a ~> b) :* s) . LitBlock <$> mapM (typedOp . runIdentity) block
OP(LitText text, s ~> Fix "L" (x :* Var [] "L" :+ Unit) :* s)

OP(AssocL, a :* b :* c ~> (a :* b) :* c)
OP(AssocR, (a :* b) :* c ~> a :* b :* c)
OP(Swap, a :* b :* c ~> b :* a :* c)
OP(SwapD, a :* b :* c :* d ~> a :* c :* b :* d)
OP(Intro1, a ~> a :* Unit)
OP(Elim1, a :* Unit ~> a)
OP(Drop, dx :* e ~> e)
  where dx = Var [Droppable] "x"
OPe(Copy, cx ~> cx :* cx)
  where cx = Var [Copyable] "x"

OPe(Apply, (x ~> xp) :* x ~> xp)
-- XXX This probably should only appear later in the pipeline
OP(ApplyTail, (x ~> xp) :* x :* Unit ~> xp)
OPe(Compose, (y ~> z) :* (x ~> y) ~> (x ~> z))
OPe(Quote, qx ~> (s ~> qx :* s))
  where qx = Var [Quotable] "x"
OPe(Relevant, (x ~> y) ~> Block btRelevant x y)
OPe(Affine, (x ~> y) ~> Block btAffine x y)

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

OPe(CondApply, bxxp :* (x :+ y) ~> xp :+ y)
  where
    bxxp = Merged (Var [Droppable] "b") $ x ~> xp
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

addTypes :: [UntypedOp] -> [TypedOp]
addTypes ops = evalState (mapM typedOp ops) emptyTCX
