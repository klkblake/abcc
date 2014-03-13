module atype

import Data.Floats

-- Use Vect n with Fin n and HasElem proofs to guarentee no holes?
data CList : List a -> Type where
  (::) : (k, Bool) -> CList ks -> CList (k :: ks)
  Nil  : CList (Nil {a})

data BlockType = MkBlockType Bool Bool

btNormal : BlockType
btNormal = MkBlockType False False

markRelevant : BlockType -> BlockType
markRelevant (MkBlockType _ a) = MkBlockType True a

markAffine : BlockType -> BlockType
markAffine (MkBlockType r _) = MkBlockType r True

union : BlockType -> BlockType -> BlockType
union (MkBlockType ra aa) (MkBlockType rb ab) = MkBlockType (ra || rb) (aa || ab)

data AType : Type -> Type where
  (.*)   : AType a -> AType a -> AType a
  (.+)   : AType a -> AType a -> AType a
  Block  : BlockType -> AType a -> AType a -> AType a
  Number : Float -> AType a
  Unit   : AType a
  Void   : AType a
  Var    : a -> AType a

infixr 0 ~>
(~>) : AType a -> AType a -> AType a
(~>) = Block btNormal

infixr 7 .*
infixr 6 .+

instance Functor AType where
  map f (x .* y) = map f x .* map f y
  map f (x .+ y) = map f x .+ map f y
  map f (Block t x y) = Block t (map f x) (map f y)
  map f (Number x) = Number x
  map f Unit = Unit
  map f Void = Void
  map f (Var x) = Var $ f x

instance Foldable AType where
  foldr f acc (x .* y) = foldr f (foldr f acc y) x
  foldr f acc (x .+ y) = foldr f (foldr f acc y) x
  foldr f acc (Block _ x y) = foldr f (foldr f acc y) x
  foldr f acc (Var k) = f k acc
  foldr _ acc _ = acc

class Observable (ty : AType a) where
  ignoreme : Int

class Droppable (ty : AType a) where
  ignoreme2 : Int

class Copyable (ty : AType a) where
  ignoreme3 : Int

class Quotable (ty : AType a) where
  ignoreme4 : Int

class Nonzero (x : Float) where
  ignoreme5 : Int

class Comparable (x : AType a) (y : AType a) where
  ignoreme6 : Int
