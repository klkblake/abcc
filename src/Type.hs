module Type where

import Control.Applicative
import Control.Monad.State.Strict
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

instance Show Type where
    showsPrec prec (a :* b) = showParen   (prec > 7) $ showsPrec 8 a . showString " * " . showsPrec 7 b
    showsPrec prec (a :+ b) = showParen   (prec > 6) $ showsPrec 7 a . showString " + " . showsPrec 6 b
    showsPrec prec (Block bt a b) =
        showBracket (prec > 0) (showsPrec 1 a . showString " -> " . showsPrec 1 b) . showBT bt
      where
        showBracket c s = if c then showChar '[' . s . showChar ']' else s
        showBT (BT False False) = id
        showBT (BT True False)  = showChar 'k'
        showBT (BT False True)  = showChar 'f'
        showBT (BT True True)   = showString "kf"
    showsPrec _    Num = showChar 'N'
    showsPrec _    Unit = showChar '1'
    showsPrec _    (Void ty) = showString "0<" . showsPrec 1 ty . showChar '>'
    showsPrec prec (Sealed seal ty) = showParen (prec > 8) $ showString "Sealed " . shows seal . showsPrec 9 ty
    showsPrec prec (Fix var ty) =
        showParen (prec > 5) $ showString "μ" . showString var . showString ". " . showsPrec 6 ty
    showsPrec _    (Var [] var) = showString var
    showsPrec prec (Var cs var) = showParen (prec > 8) $ showString "Var " . shows cs . showChar ' ' . showString var
    showsPrec _    (Merged a b) = showChar '{' . showsPrec 1 a . showString " ∧ " . showsPrec 1 b . showChar '}'

(~>) :: Type -> Type -> Type
(~>) = Block btNormal

infixr 7 :*
infixr 6 :+
infixr 1 ~>

data TypeContext = TypeContext { tcx_used        :: Map String Int
                               , tcx_constraints :: Map String [Constraint]
                               }

emptyTCX :: TypeContext
emptyTCX = TypeContext Map.empty Map.empty

fresh :: String -> State TypeContext String
fresh var = do
    let var' = dropWhileEnd (`elem` ['0'..'9']) var
    n <- Map.findWithDefault 0 var <$> gets tcx_used
    modify $ \tcx -> tcx { tcx_used = Map.insert var (n + 1) $ tcx_used tcx }
    return $ var' ++ show n
