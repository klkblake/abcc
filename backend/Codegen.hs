module Codegen where

import Op
import Type

compileOp :: Type -> Op -> Either String (String, Type)
compileOp (Pair a (Pair b c))          AssocL   = let ty = Pair (Pair a b) c          in Right ("Shifted left: "    ++ show ty, ty)
compileOp (Pair (Pair a b) c)          AssocR   = let ty = Pair a (Pair b c)          in Right ("Shifted right: "   ++ show ty, ty)
compileOp (Pair a (Pair b c))          Swap     = let ty = Pair b (Pair a c)          in Right ("Swapped: "         ++ show ty, ty)
compileOp (Pair a (Pair b (Pair c d))) SwapD    = let ty = Pair a (Pair c (Pair b d)) in Right ("Swapped second: "  ++ show ty, ty)
compileOp a                            Intro1   = let ty = Pair a Unit                in Right ("Introduced unit: " ++ show ty, ty)
compileOp (Pair a Unit)                Elim1    = let ty = a                          in Right ("Eliminated unit: " ++ show ty, ty)
compileOp (Pair x e)                   Drop     = let ty = e                          in Right ("Dropped: "         ++ show ty, ty)
compileOp (Pair x e)                   Copy     = let ty = (Pair x (Pair x e))        in Right ("Copied: "          ++ show ty, ty)
compileOp (Pair Num (Pair Num e))      Add      = let ty = Pair Num e                 in Right ("Added: "           ++ show ty, ty)
compileOp (Pair Num (Pair Num e))      Multiply = let ty = Pair Num e                 in Right ("Multiplied: "      ++ show ty, ty)
compileOp ty op = Left $ "ERROR: type " ++ show ty ++ " does not match operator " ++ show op

compile :: [Op] -> Either String String
compile ops = case compile' (Pair (Pair Num Unit) Unit) ops of
                  Right (compiled, Pair (Pair Num Unit) Unit) -> Right compiled
                  Right (compiled, ty) -> Left $ "\n\tOutput type: " ++ show ty ++ "\n\tCompiled:\n" ++ compiled
                  Left err -> Left err
  where
    compile' ty (op:ops) = do
        (compiled, outTy) <- compileOp ty op
        (rest, outTy') <- compile' outTy ops
        return (compiled ++ "\n" ++ rest, outTy')
    compile' ty [] = return ("", ty)
