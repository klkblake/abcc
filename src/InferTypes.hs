module InferTypes where

import Type
import Op

data Typed a = Typed Type a
             deriving Show

a, a', b, c, d, e, s, x, x', y, z :: Type
a = Var [] "a"
a' = Var [] "a'"
b = Var [] "b"
c = Var [] "c"
d = Var [] "d"
e = Var [] "e"
s = Var [] "s"
x = Var [] "x"
x' = Var []"x'"
y = Var [] "y"
z = Var [] "z"

opType :: Op -> Type
-- XXX Temporary and obviously wrong
opType (LitBlock _) = s ~> (a ~> b) :* s
opType (LitText _) = s ~> Fix "L" (x :* Var [] "L" :+ Unit) :* s

opType AssocL = a :* b :* c ~> (a :* b) :* c
opType AssocR = (a :* b) :* c ~> a :* b :* c
opType Swap   = a :* b :* c ~> b :* a :* c
opType SwapD  = a :* b :* c :* d ~> a :* c :* b :* d
opType Intro1 = a ~> a :* Unit
opType Elim1  = a :* Unit ~> a
opType Drop   = dx :* e ~> e
  where dx = Var [Droppable] "x"
opType Copy   = cx :* e ~> cx :* cx :* e
  where cx = Var [Copyable] "x"

opType Apply    = (x ~> x') :* x :* e ~> x' :* e
-- XXX This probably should only appear later in the pipeline
opType ApplyTail = (x ~> x') :* x :* Unit ~> x'
opType Compose  = (y ~> z) :* (x ~> y) :* e ~> (x ~> z) :* e
opType Quote    = qx :* e ~> (s ~> qx :* s) :* e
  where qx = Var [Quotable] "x"
opType Relevant = (x ~> y) :* e ~> (Block btRelevant x y) :* e
opType Affine   = (x ~> y) :* e ~> (Block btAffine x y) :* e

opType IntroNum = e ~> Num :* e
opType (Digit _) =  Num :* e ~> Num :* e

opType Add = Num :* Num :* e ~> Num :* e
opType Multiply = Num :* Num :* e ~> Num :* e
opType Inverse = Num :* e ~> Num :* e
opType Negate = Num :* e ~> Num :* e
opType Divmod = Num :* Num :* e ~> Num :* Num :* e

opType AssocLS = (a :+ b :+ c) :* e ~> ((a :+ b) :+ c) :* e
opType AssocRS = ((a :+ b) :+ c) :* e ~> (a :+ b :+ c) :* e
opType SwapS   = (a :+ b :+ c) :* e ~> (b :+ a :+ c) :* e
opType SwapDS  = (a :+ b :+ c :+ d) :* e ~> (a :+ c :+ b :+ d) :* e
opType Intro0 = a :* e ~> (a :+ Void x) :* e
opType Elim0  = (a :+ Void x) :* e ~> a :* e

opType CondApply = bxx' :* (x :+ y) :* e ~> (x' :+ y) :* e
  where
    bxx' = Merged (Var [Droppable] "b") $ x ~> x'
opType Distrib = a :* (b :+ c) :* e ~> (a :* b :+ a :* c) :* e
opType Factor = (a :* b :+ c :* d) :* e ~> (a :+ c) :* (b :+ d) :* e
opType Merge = (a :+ a') :* e ~> (Merged a a') :* e
opType Assert = (a :+ b) :* e ~> b :* e

opType Greater = Num :* Num :* e ~> (Num :* Num :+ Num :* Num) :* e

opType (Sealer seal) = a ~> Sealed seal a
opType (Unsealer seal) = Sealed seal a ~> a

opType AssertEQ = a :* b :* e ~> a :* b :* e
opType DebugPrintRaw = error "DebugPrintRaw not supported. I need to implement the standard debug print system"
opType DebugPrintText = error "DebugPrintText not supported. I need to implement the standard debug print system"

typedOp :: Op -> Typed Op
typedOp op = Typed (opType op) op

inferTypes :: [Op] -> [Typed Op]
inferTypes ops = map typedOp ops
