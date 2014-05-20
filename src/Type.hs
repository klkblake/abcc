module Type where

data Constraint = Droppable | Copyable | Quotable
                deriving Show

data BlockType = BT Bool Bool
               deriving Show

btNormal, btRelevant, btAffine :: BlockType
btNormal = BT False False
btRelevant = BT True False
btAffine = BT False True

data Type = (:*) Type Type
          | (:+) Type Type
          | Block BlockType Type Type
          | Num
          | Unit
          | Void Type
          | Sealed String Type
          | Fix String Type
          | Var [Constraint] String
          | Merged Type Type
          deriving Show

(~>) :: Type -> Type -> Type
(~>) = Block btNormal

infixr 7 :*
infixr 6 :+
infixr 1 ~>
