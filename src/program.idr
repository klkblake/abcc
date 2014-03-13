module program

import atype

TextType : String -> AType a
TextType str = foldr build (Number 3) $ unpack str
  where
    build c = (.*) (Number . cast $ ord c)

DivModType : Float -> Float -> AType a -> AType a
DivModType a b e = let q = floor $ a / b
                   in Number (a - q * b) .* Number q .* e

IsPairType : AType a -> AType a -> AType a
IsPairType free (x .* y) = free .+ x .* y
IsPairType free x         = x .+ free 

IsSumType : AType a -> AType a -> AType a
IsSumType free (x .+ y) = free .+ x .+ y
IsSumType free x         = x .+ free 

IsBlockType : AType a -> AType a -> AType a
IsBlockType free (x ~> y) = free .+ (x ~> y)
IsBlockType free x        = x .+ free 

IsNumberType : AType a -> AType a -> AType a
IsNumberType free (Number x) = free .+ (Number x)
IsNumberType free x          = x .+ free 

mutual
  -- We only need to predeclare the ones that appear bare.
  using (a : AType v, e : AType v)
    data Op : AType v -> AType v -> Type where
      IntroBlock  : Program a b -> Op e ((a ~> b) .* e)
      IntroText   : (str : String) -> Op e (TextType str .* e)

      AssocL : Op (a .* b .* c) ((a .* b) .* c)
      AssocR : Op ((a .* b) .* c) (a .* b .* c)
      Swap   : Op (a .* b .* c) (b .* a .* c)
      SwapD  : Op (a .* b .* c .* d) (a .* c .* b .* d)
      Intro1 : Op a (a .* Unit)
      Elim1  : Op (a .* Unit) a
      Drop   : Droppable x => Op (x .* e) e
      Copy   : Copyable x  => Op (x .* e) (x .* x .* e)

      Apply   : Op (Block t x x' .* x .* e) (x' .* e)
      Compose : Op (Block t2 y z .* Block t1 x y .* e) (Block (union t1 t2) x z .* e)
      Quote   : Quotable x => Op (x .* e) ((s ~> x .* s) .* e)
      Keep    : Op (Block t x y .* e) (Block (markRelevant t) x y .* e)
      Affine  : Op (Block t x y .* e) (Block (markAffine t) x y .* e)

      IntroNumber : Op e (Number 0 .* e)
      Digit       : (d : Float) -> Op (Number n .* e) (Number (10 * n + d) .* e)

      Add      : Op (Number n .* Number m .* e) (Number (n + m) .* e)
      Multiply : Op (Number n .* Number m .* e) (Number (n * m) .* e)
      Inverse  : (so (n /= 0)) -> Op (Number n .* e) (Number (1 / n) .* e)
      Negate   : Op (Number n .* e) (Number (-n) .* e)
      DivMod   : (so (n /= 0)) -> Op (Number m .* Number n .* e) (DivModType n m e)

      AssocLS : Op ((a .+ b .+ c) .* e) (((a .+ b) .+ c) .* e)
      AssocRS : Op (((a .+ b) .+ c) .* e) ((a .+ b .+ c) .* e)
      SwapS   : Op ((a .+ b .+ c) .* e) ((b .+ a .+ c) .* e)
      SwapDS  : Op ((a .+ b .+ c .+ d) .* e) ((a .+ c .+ b .+ d) .* e)
      Intro0  : Op (a .* e) ((a .+ Void) .* e)
      Elim0   : Op ((a .+ Void) .* e) (a .* e)

      CondApply : Droppable (Block t x x') => Op (Block t x x' .* (x .+ y) .* e) ((x' .+ y) .* e)
      Distrib   : Op (a .* (b .+ c) .* e) ((a .* b .+ a .* c) .* e)
      Factor    : Op ((a .* b .+ c .* d) .* e) ((a .+ c) .* (b .+ d) .* e)
      Merge     : Op ((a .+ a) .* e) (a .* e) -- stricter than necessary
      Assert    : Op ((a .+ b) .* e) (b .* e)

      IsPair   : Observable x => Op x (IsPairType a x)
      IsSum    : Observable x => Op x (IsSumType a x)
      IsBlock  : Observable x => Op x (IsBlockType a x)
      IsNumber : Observable x => Op x (IsNumberType a x)
      Compare  : Comparable x y => Op (x .* y .* e) ((y .* x .+ x .* y) .* e)
  {-
    ProgramConsType : AType Nat -> AType Nat -> AType Nat -> AType Nat -> Type
    ProgramConsType a b c d =
      let base = foldr maximum 0 (a .* b) + 1
          c' = map (+base) c
          d' = map (+base) d
          (subs, subs') = unify b c'
          a'  = map (lookup subs) a
          b'  = map (lookup subs) b
          c'' = map (lookup subs') c'
          d'' = map (lookup subs') d'
      Op a b -> Program c d -> (b' = c'') -> Program a' d'
  -}

    data Program : AType v -> AType v -> Type where
      --(::) : {a, b, c, d : AType Nat} -> ProgramConsType a b c d
      (::) : {a, b, c : AType Nat} -> Op a b -> Program b c -> Program a c
      Nil : {a : AType Nat} -> Program a a
