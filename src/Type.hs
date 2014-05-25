module Type where

import Control.Applicative
import Control.Monad.State
import Data.List (dropWhileEnd, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data Constraint = Droppable | Copyable | Quotable
                deriving (Eq, Show)

data Type = (:*) Type Type
          | (:+) Type Type
          | (:~>) Type Type
          | Num
          | Unit
          | Void Type
          | Sealed String Type
          | Fix String Type
          | Var String
          | Merged Type Type

instance Show Type where
    showsPrec prec (a :* b) = showParen   (prec > 7) $ showsPrec 8 a . showString " * " . showsPrec 7 b
    showsPrec prec (a :+ b) = showParen   (prec > 6) $ showsPrec 7 a . showString " + " . showsPrec 6 b
    showsPrec prec (a :~> b) =
        showBracket (prec > 0) (showsPrec 1 a . showString " -> " . showsPrec 1 b)
      where
        showBracket c s = if c then showChar '[' . s . showChar ']' else s
    showsPrec _    Num = showChar 'N'
    showsPrec _    Unit = showChar '1'
    showsPrec _    (Void ty) = showString "0<" . showsPrec 1 ty . showChar '>'
    showsPrec prec (Sealed seal ty) = showParen (prec > 8) $ showString "Sealed " . shows seal . showsPrec 9 ty
    showsPrec prec (Fix var ty) =
        showParen (prec > 5) $ showString "μ" . showString var . showString ". " . showsPrec 6 ty
    showsPrec _    (Var var) = showString var
    showsPrec _    (Merged a b) = showChar '{' . showsPrec 1 a . showString " ∧ " . showsPrec 1 b . showChar '}'

(~>) :: Type -> Type -> Type
(~>) = (:~>)

infixr 7 :*
infixr 6 :+
infixr 1 :~>
infixr 1 ~>

data TypeContext = TypeContext { tcx_used        :: Map String Int
                               , tcx_constraints :: Map String [Constraint]
                               }
                 deriving Show

emptyTCX :: TypeContext
emptyTCX = TypeContext Map.empty Map.empty

fresh :: String -> State TypeContext String
fresh var = do
    let var' = dropWhileEnd (`elem` ['0'..'9']) var
    n <- Map.findWithDefault 0 var <$> gets tcx_used
    modify $ \tcx -> tcx { tcx_used = Map.insert var (n + 1) $ tcx_used tcx }
    return $ var' ++ show n

constrain :: String -> Constraint -> State TypeContext ()
constrain var c = modify $ \tcx -> tcx { tcx_constraints = Map.alter putSet var $ tcx_constraints tcx }
  where
    putSet = Just . nub . (c:) . fromMaybe []
