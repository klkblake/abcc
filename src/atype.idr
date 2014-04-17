module atype

import Data.Floats
import Data.Vect

--replace' : DecEq a => a -> a -> Vect n a -> Vect n a
replace' : String -> String -> Vect n String -> Vect n String
replace' x y (z :: xs) with (decEq x z)
  replace' x y (x :: xs) | Yes refl = y :: xs
  replace' x y (_ :: xs) | No  _    = x :: replace' x y xs
replace' x y [] = []

data Constraint : {a : Type} -> (k : a) -> Type where
  MkConstraint : k -> Bool -> Constraint k

-- Use Vect n with Fin n and HasElem proofs to guarentee no holes?
data CList : Vect n a -> Type where
  (::) : {k : a} -> {ks : Vect n a} -> Constraint k -> CList ks -> CList (k :: ks)
  Nil  : CList (Nil {a})

data BlockType = MkBlockType Bool Bool

instance Show BlockType where
  show (MkBlockType r a) = boolElim r "r" "" ++ boolElim a "a" ""

pairToBT : (a, b) = (a', b') -> MkBlockType a b = MkBlockType a' b'
pairToBT refl = refl

btToPair : MkBlockType a b = MkBlockType a' b' -> (a, b) = (a', b')
btToPair refl = refl

instance DecEq BlockType where
  decEq (MkBlockType r a) (MkBlockType r' a') = case decEq (r, a) (r', a') of
                                                     Yes prf => Yes $ pairToBT prf
                                                     No  prf => No  $ \eq => prf $ btToPair eq

btNormal : BlockType
btNormal = MkBlockType False False

markRelevant : BlockType -> BlockType
markRelevant (MkBlockType _ a) = MkBlockType True a

markAffine : BlockType -> BlockType
markAffine (MkBlockType r _) = MkBlockType r True

union : BlockType -> BlockType -> BlockType
union (MkBlockType ra aa) (MkBlockType rb ab) = MkBlockType (ra || rb) (aa || ab)

using (vs : Vect n String)
  data OType : Vect n String -> Type where
    (.*)   : OType vs -> OType vs -> OType vs
    (.+)   : OType vs -> OType vs -> OType vs
    Block  : BlockType -> OType vs -> OType vs -> OType vs
    Number : Float -> OType vs
    Unit   : OType vs
    Void   : OType vs
    Var    : (v : String) -> {default tactics { applyTactic findElem 10; solve; } prf : Elem v vs} -> OType vs

  infixr 0 ~>
  (~>) : OType vs -> OType vs -> OType vs
  (~>) = Block btNormal

  infixr 7 .*
  infixr 6 .+

  showParen : Show a => Bool -> a -> String
  showParen c x = if c then "(" ++ show x ++ ")" else show x

  showPrec : Nat -> OType vs -> String
  showPrec prec (x .* y)      = showParen   (prec > 2) $ showPrec 3 x ++ " * "    ++ showPrec 2 y
  showPrec prec (x .+ y)      = showParen   (prec > 1) $ showPrec 2 x ++ " + "  ++ showPrec 1 y
  showPrec _    (Block t x y) = "[" ++ showPrec 0 x ++ " -> " ++ showPrec 0 y ++ "]" ++ show t
  showPrec _    (Number x)    = "N(" ++ show x ++ "]"
  showPrec _    Unit          = "1"
  showPrec _    Void          = "0"
  showPrec _    (Var v)       = v

  instance Show (OType vs) where
    show x = showPrec 0 x

  weakenElem : Elem v vs -> Elem v (x :: vs)
  weakenElem Here = There Here
  weakenElem (There x) = There (There x)

  weaken : OType vs -> OType (v :: vs)
  weaken (x .* y) = weaken x .* weaken y
  weaken (x .+ y) = weaken x .+ weaken y
  weaken (Block t x y) = Block t (weaken x) (weaken y)
  weaken (Number x) = Number x
  weaken Unit = Unit
  weaken Void = Void
  weaken (Var v {prf}) = Var v {prf=weakenElem prf}

  mutual
    %logging 5
    renameElem : (a : String) -> (b : String) -> Elem a vs -> Elem b (replace' {n=n} a b vs)
    renameElem a b Here = ?renameElem_rhs_1
    renameElem a b (There x) = ?renameElem_rhs_2
    %logging 0

    renameElem' : (a : String) -> (b : String) -> Elem v vs -> Elem v (replace' a b vs)

  renameVar : (a : String) -> (b : String) -> OType vs -> OType (replace' a b vs)
  renameVar a b (x .* y) = renameVar a b x .* renameVar a b y
  renameVar a b (x .+ y) = renameVar a b x .+ renameVar a b y
  renameVar a b (Block t x y) = Block t (renameVar a b x) (renameVar a b y)
  renameVar a b (Number x) = Number x
  renameVar a b Unit = Unit
  renameVar a b Void = Void
  renameVar a b (Var v {prf}) with (decEq a v)
    renameVar a b (Var a {prf}) | Yes refl = Var b {prf=renameElem a b prf}
    renameVar a b (Var v {prf}) | No _ = Var v {prf=renameElem' a b prf}

  unwrapTheres : {a : Elem v vs} -> {b : Elem v vs} -> There {y=y} a = There {y=y} b -> a = b
  unwrapTheres refl = refl

  hereNotThere : {a : Elem v vs} -> Here {x=v} {xs=vs} = There {x=v} {xs=vs} {y=y} a -> _|_
  hereNotThere refl impossible

  instance DecEq (Elem v vs) where
    decEq Here Here = Yes refl
    decEq (There x) (There y) = case decEq x y of
                                     Yes prf => Yes $ cong prf
                                     No  prf => No $ \eq => prf $ unwrapTheres eq
    decEq Here (There x) = No hereNotThere
    decEq (There x) Here = No $ negEqSym hereNotThere

  using (a, b, a', b' : OType vs)
    pairToProduct : (a, b) = (a', b') -> a .* b = a' .* b'
    pairToProduct refl = refl

    productToPair : a .* b = a' .* b' -> (a, b) = (a', b')
    productToPair refl = refl

    pairToSum : (a, b) = (a', b') -> a .+ b = a' .+ b'
    pairToSum refl = refl

    sumToPair : a .+ b = a' .+ b' -> (a, b) = (a', b')
    sumToPair refl = refl

    pairToBlock : (t = t') -> (a, b) = (a', b') -> Block t a b = Block t' a' b'
    pairToBlock refl refl = refl

    blockToPair : Block t a b = Block t' a' b' -> (a, b) = (a', b')
    blockToPair refl = refl

    blockTypes : Block t a b = Block t' a' b' -> t = t'
    blockTypes refl = refl

  unwrapNums : Number {vs=vs} x = Number {vs=vs} x' -> x = x'
  unwrapNums refl = refl

  varNames : {p : Elem v vs} -> {p' : Elem v' vs} -> Var v {prf=p} = Var v' {prf=p'} -> v = v'
  varNames refl = refl

  varProofs : {p : Elem v vs} -> {p' : Elem v' vs} -> Var v {prf=p} = Var v' {prf=p'} -> p = p'
  varProofs refl = refl

  -- XXX figure out how to remove instances of believe_me and assert_total
  instance DecEq (OType vs) where
    decEq (x .* y) (x' .* y') = case assert_total $ decEq (x, y) (x', y') of
                                     Yes prf => Yes $ pairToProduct prf
                                     No  prf => No $ \eq => prf $ productToPair eq
    decEq (x .* y) z = No believe_me
    decEq (x .+ y) (x' .+ y') = case assert_total $ decEq (x, y) (x', y') of
                                     Yes prf => Yes $ pairToSum prf
                                     No  prf => No $ \eq => prf $ sumToPair eq
    decEq (_ .+ _) _ = No believe_me
    decEq (Block t x y) (Block t' x' y') with (decEq t t')
      decEq (Block t x y) (Block t x' y')  | Yes prf = case assert_total $ decEq (x, y) (x', y') of
                                                            Yes prf' => Yes $ pairToBlock prf prf'
                                                            No  prf' => No $ \eq => prf' $ blockToPair eq
      decEq (Block t x y) (Block t' x' y') | No  prf = No $ \eq => prf $ blockTypes eq
    decEq (Block _ _ _) _ = No believe_me
    decEq (Number x) (Number x') = case decEq x x' of
                                        Yes prf => Yes $ cong prf
                                        No  prf => No $ \eq => prf $ unwrapNums eq
    decEq (Number _) _ = No believe_me
    decEq Unit Unit = Yes refl
    decEq Unit _ = No believe_me
    decEq Void Void = Yes refl
    decEq Void _ = No believe_me
    decEq (Var v {prf=p}) (Var v' {prf=p'}) with (decEq v v')
      decEq (Var v {prf=p}) (Var v  {prf=p'}) | Yes refl with (decEq p p')
        decEq (Var v {prf=p}) (Var v {prf=p})  | Yes refl | Yes refl = Yes refl
        decEq (Var v {prf=p}) (Var v {prf=p'}) | Yes refl | No  prf  = No $ \eq => prf $ varProofs eq
      decEq (Var v {prf=p}) (Var v' {prf=p'}) | No  prf = No $ \eq => prf $ varNames eq
    decEq (Var _) _ = No believe_me

  data FType : Vect n String -> Type where
    (~~>) : OType vs -> OType vs -> FType vs
  infixr 0 ~~>

  data QType : Type where
    Forall : (vs : Vect n String) -> FType vs -> QType

  --instance Functor AType where
  --  map f (x .* y) = map f x .* map f y
  --  map f (x .+ y) = map f x .+ map f y
  --  map f (Block t x y) = Block t (map f x) (map f y)
  --  map f (Number x) = Number x
  --  map f Unit = Unit
  --  map f Void = Void
  --  map f (Var x) = Var $ f x

  --instance Foldable AType where
  --  foldr f acc (x .* y) = foldr f (foldr f acc y) x
  --  foldr f acc (x .+ y) = foldr f (foldr f acc y) x
  --  foldr f acc (Block _ x y) = foldr f (foldr f acc y) x
  --  foldr f acc (Var k) = f k acc
  --  foldr _ acc _ = acc

  class Droppable (ty : OType vs) where
    ignoreme2 : Int

  class Copyable (ty : OType vs) where
    ignoreme3 : Int

  class Quotable (ty : OType vs) where
    ignoreme4 : Int

  class Nonzero (x : Float) where
    ignoreme5 : Int

  class Comparable (x : OType vs) (y : OType vs) where
    ignoreme6 : Int
