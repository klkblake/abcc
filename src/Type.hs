module Type where

import Control.Applicative
import Control.Monad.State
import Data.List (dropWhileEnd, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data Constraint = Droppable | Copyable | Quotable
                deriving (Eq, Show)

data FlagExpr = FVar String
              | FLit Bool
              | FOr FlagExpr FlagExpr
              | FAffine Type
              | FRelevant Type
              deriving (Eq, Show)

data Type = (:*) Type Type
          | (:+) Type Type
          | Block FlagExpr FlagExpr Type Type
          | Num
          | Unit
          | Void Type
          | Sealed String Type
          | Fix String Type
          | Var String
          | Merged Type Type
          deriving Eq

instance Show Type where
    showsPrec prec (a :* b) = showParen   (prec > 7) $ showsPrec 8 a . showString " * " . showsPrec 7 b
    showsPrec prec (a :+ b) = showParen   (prec > 6) $ showsPrec 7 a . showString " + " . showsPrec 6 b
    showsPrec _ (Block aff rel a b) =
        showChar '[' . showsPrec 1 a . showString " -> " . showsPrec 1 b . showChar ']' . showAff aff . showRel rel
      where
        showAff (FLit True) = showChar 'f'
        showAff (FLit False) = id
        showAff _ = showChar '{' . showsPrec 0 aff . showChar '}'
        showRel (FLit True) = showChar 'r'
        showRel (FLit False) = id
        showRel _ = showChar '{' . showsPrec 0 rel . showChar '}'
    showsPrec _    Num = showChar 'N'
    showsPrec _    Unit = showChar '1'
    showsPrec _    (Void ty) = showString "0<" . showsPrec 1 ty . showChar '>'
    showsPrec prec (Sealed seal ty) = showParen (prec > 8) $ showString "Sealed " . shows seal . showsPrec 9 ty
    showsPrec prec (Fix var ty) =
        showParen (prec > 5) $ showString "μ" . showString var . showString ". " . showsPrec 6 ty
    showsPrec _    (Var var) = showString var
    showsPrec _    (Merged a b) = showChar '{' . showsPrec 1 a . showString " ∧ " . showsPrec 1 b . showChar '}'

infixr 7 :*
infixr 6 :+

data TypeContext = TypeContext { tcx_used        :: Map String Int
                               , tcx_constraints :: Map String [Constraint]
                               }
                 deriving Show

emptyTCX :: TypeContext
emptyTCX = TypeContext Map.empty Map.empty

fresh :: (Functor m, Monad m) => String -> StateT TypeContext m String
fresh var = do
    let var' = dropWhileEnd (`elem` ['0'..'9']) var
    n <- Map.findWithDefault 0 var <$> gets tcx_used
    modify $ \tcx -> tcx { tcx_used = Map.insert var (n + 1) $ tcx_used tcx }
    return $ var' ++ show n

modifyConstraints :: Monad m => (Map String [Constraint] -> Map String [Constraint]) -> StateT TypeContext m ()
modifyConstraints f = modify $ \tcx -> tcx { tcx_constraints = f $ tcx_constraints tcx }

constrain :: Monad m => String -> Constraint -> StateT TypeContext m ()
constrain var c = modifyConstraints $ Map.alter putSet var
  where
    putSet = Just . nub . (c:) . fromMaybe []
