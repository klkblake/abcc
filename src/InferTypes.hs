{-# LANGUAGE CPP #-}
module InferTypes where

import Data.Functor.Identity

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

renameType :: Type -> Type
renameType ty = ty

#define OP(op, ty) typedOp (op) = Typed (renameType $ ty) $ op

typedOp :: UntypedOp -> TypedOp
typedOp (LitBlock block) = Typed (s ~> (a ~> b) :* s) . LitBlock $ map (typedOp . runIdentity) block
OP(LitText text, s ~> Fix "L" (x :* Var [] "L" :+ Unit) :* s)

OP(AssocL, a :* b :* c ~> (a :* b) :* c)
OP(AssocR, (a :* b) :* c ~> a :* b :* c)
OP(Swap, a :* b :* c ~> b :* a :* c)
OP(SwapD, a :* b :* c :* d ~> a :* c :* b :* d)
OP(Intro1, a ~> a :* Unit)
OP(Elim1, a :* Unit ~> a)
OP(Drop, dx :* e ~> e)
  where dx = Var [Droppable] "x"
OP(Copy, cx :* e ~> cx :* cx :* e)
  where cx = Var [Copyable] "x"

OP(Apply, (x ~> xp) :* x :* e ~> xp :* e)
-- XXX This probably should only appear later in the pipeline
OP(ApplyTail, (x ~> xp) :* x :* Unit ~> xp)
OP(Compose, (y ~> z) :* (x ~> y) :* e ~> (x ~> z) :* e)
OP(Quote, qx :* e ~> (s ~> qx :* s) :* e)
  where qx = Var [Quotable] "x"
OP(Relevant, (x ~> y) :* e ~> (Block btRelevant x y) :* e)
OP(Affine, (x ~> y) :* e ~> (Block btAffine x y) :* e)

OP(IntroNum, e ~> Num :* e)
OP(Digit digit, Num :* e ~> Num :* e)

OP(Add, Num :* Num :* e ~> Num :* e)
OP(Multiply, Num :* Num :* e ~> Num :* e)
OP(Inverse, Num :* e ~> Num :* e)
OP(Negate, Num :* e ~> Num :* e)
OP(Divmod, Num :* Num :* e ~> Num :* Num :* e)

OP(AssocLS, (a :+ b :+ c) :* e ~> ((a :+ b) :+ c) :* e)
OP(AssocRS, ((a :+ b) :+ c) :* e ~> (a :+ b :+ c) :* e)
OP(SwapS, (a :+ b :+ c) :* e ~> (b :+ a :+ c) :* e)
OP(SwapDS, (a :+ b :+ c :+ d) :* e ~> (a :+ c :+ b :+ d) :* e)
OP(Intro0, a :* e ~> (a :+ Void x) :* e)
OP(Elim0, (a :+ Void x) :* e ~> a :* e)

OP(CondApply, bxxp :* (x :+ y) :* e ~> (xp :+ y) :* e)
  where
    bxxp = Merged (Var [Droppable] "b") $ x ~> xp
OP(Distrib, a :* (b :+ c) :* e ~> (a :* b :+ a :* c) :* e)
OP(Factor, (a :* b :+ c :* d) :* e ~> (a :+ c) :* (b :+ d) :* e)
OP(Merge, (a :+ ap) :* e ~> (Merged a ap) :* e)
OP(Assert, (a :+ b) :* e ~> b :* e)

OP(Greater, Num :* Num :* e ~> (Num :* Num :+ Num :* Num) :* e)

OP(Sealer seal, a ~> Sealed seal a)
OP(Unsealer seal, Sealed seal a ~> a)

OP(AssertEQ, a :* b :* e ~> a :* b :* e)
OP(DebugPrintRaw, error "DebugPrintRaw not supported. I need to implement the standard debug print system")
OP(DebugPrintText, error "DebugPrintText not supported. I need to implement the standard debug print system")

#undef OP

inferTypes :: [UntypedOp] -> [TypedOp]
inferTypes ops = map typedOp ops
