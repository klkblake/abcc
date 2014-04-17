module program

import atype

using (vs : Vect n String)
  TextType : String -> OType vs
  TextType str = foldr build Unit $ unpack str
    where
      build c x = Number (cast $ ord c) .* (x .+ Unit)

  DivModType : Float -> Float -> OType vs -> OType vs
  DivModType a b e = let q = floor $ a / b
                     in Number (a - q * b) .* Number q .* e

  mutual
      forall : (vs : Vect n String) -> FType vs -> Type
      forall vs x = Op $ Forall vs x

      va : {default tactics { applyTactic findElem 10; solve; } prf : Elem "a" vs} -> OType vs
      va {prf} = Var "a" {prf}

      vb : {default tactics { applyTactic findElem 10; solve; } prf : Elem "b" vs} -> OType vs
      vb {prf} = Var "b" {prf}

      vc : {default tactics { applyTactic findElem 10; solve; } prf : Elem "c" vs} -> OType vs
      vc {prf} = Var "c" {prf}

    -- We only need to predeclare the ones that appear bare.
    --using (a : AType va, e : AType ve)
      public
      data Op : QType -> Type where
        --IntroBlock  : {a, b : OType vs} -> Program (Forall vs $ a ~~> b) -> forall ("e"::vs) $ Var "e" ~~> weaken (a ~> b) .* Var "e"
        --IntroText   : (str : String) -> forall ["e"] $ Var "e" ~~> TextType str .* Var "e"

        AssocL : forall ["a", "b", "c"] $ va .* vb .* vc ~~> va .* vb .* vc
        AssocR : forall ["a", "b", "c"] $ (va .* vb) .* vc ~~> va .* vb .* vc
        Swap   : forall ["a", "b", "c"] $ va .* vb .* vc ~~> vb .* va .* vc
        SwapD  : forall ["a", "b", "c", "d"] $ va .* vb .* vc .* vd ~~> va .* vc .* vb .* vd
        {-
        Intro1 : Op (a ~> a .* Unit)
        Elim1  : Op (a .* Unit ~> a)
        Drop   : Droppable x => Op (x .* e ~> e)
        Copy   : Copyable x  => Op (x .* e ~> x .* x .* e)

        Apply   : Op (Block t x x' .* x .* e ~> x' .* e)
        Compose : Op (Block t2 y z .* Block t1 x y .* e ~> Block (union t1 t2) x z .* e)
        Quote   : Quotable x => Op (x .* e ~> (s ~> x .* s) .* e)
        Keep    : Op (Block t x y .* e ~> Block (markRelevant t) x y .* e)
        Affine  : Op (Block t x y .* e ~> Block (markAffine t) x y .* e)

        IntroNumber : Op (e ~> Number 0 .* e)
        Digit       : (d : Float) -> Op (Number n .* e ~> Number (10 * n + d) .* e)

        Add      : Op (Number n .* Number m .* e ~> Number (n + m) .* e)
        Multiply : Op (Number n .* Number m .* e ~> Number (n * m) .* e)
        Inverse  : (so (n /= 0)) -> Op (Number n .* e ~> Number (1 / n) .* e)
        Negate   : Op (Number n .* e ~> Number (-n) .* e)
        DivMod   : (so (n /= 0)) -> Op (Number m .* Number n .* e ~> DivModType n m e)

        AssocLS : Op ((a .+ b .+ c) .* e ~> ((a .+ b) .+ c) .* e)
        AssocRS : Op (((a .+ b) .+ c) .* e ~> (a .+ b .+ c) .* e)
        SwapS   : Op ((a .+ b .+ c) .* e ~> (b .+ a .+ c) .* e)
        SwapDS  : Op ((a .+ b .+ c .+ d) .* e ~> (a .+ c .+ b .+ d) .* e)
        Intro0  : Op (a .* e ~> (a .+ Void) .* e)
        Elim0   : Op ((a .+ Void) .* e ~> a .* e)

        CondApply : Droppable (Block t x x') => Op (Block t x x' .* (x .+ y) .* e ~> (x' .+ y) .* e)
        Distrib   : Op (a .* (b .+ c) .* e ~> (a .* b .+ a .* c) .* e)
        Factor    : Op ((a .* b .+ c .* d) .* e ~> (a .+ c) .* (b .+ d) .* e)
        Merge     : Op ((a .+ a) .* e ~> a .* e) -- stricter than necessary
        Assert    : Op ((a .+ b) .* e ~> b .* e)

        Compare  : Comparable x y => Op (x .* y .* e ~> (y .* x .+ x .* y) .* e)
        -}
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

      -- Carry detailed types via proofs? map subsL a = map subsR b?
      public
      data Program : QType -> Type where
        --(::) : {a, b, c, d : AType Nat} -> ProgramConsType a b c d
        (::) : {a, b, c : OType vs} -> Op (Forall vs (a ~~> b)) -> Program (Forall vs (b ~~> c)) -> Program (Forall vs (a ~~> c))
        Nil : {a : OType vs} -> Program (Forall vs $ a ~~> a)

  mutual
    instance Show (Op ty) where
      --show (IntroBlock b) = "[" ++ show b ++ "]"
      --show (IntroText str) = "\"" ++ (pack . intercalate ['\n', ' '] . split (== '\n') . unpack $ str) ++ "\n~"
      show AssocL = "l"
      show AssocR = "r"
      show Swap = "w"
      show SwapD = "z"

    instance Show (Program ty) where
      show (x :: y) = show x ++ " " ++ show y
      show [] = ""
