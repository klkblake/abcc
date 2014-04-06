module Codegen where

import qualified LLVM.General.AST as LLVM
import LLVM.General.AST hiding (Type, Add)

import Op
import Type

double :: LLVM.Type
double = FloatingPointType 64 IEEE

header = "#include \"ops.h\"\n\nAny run_abc(Any v) {\n"
footer = "\treturn v;\n}\n"
run op = "\tv = " ++ op ++ "(v);\n"

compileOp :: Type -> Op -> Either String (String, Type)
compileOp (Pair a (Pair b c))          AssocL   = let ty = Pair (Pair a b) c          in Right (run "assocl",   ty)
compileOp (Pair (Pair a b) c)          AssocR   = let ty = Pair a (Pair b c)          in Right (run "assocr",   ty)
compileOp (Pair a (Pair b c))          Swap     = let ty = Pair b (Pair a c)          in Right (run "swap",     ty)
compileOp (Pair a (Pair b (Pair c d))) SwapD    = let ty = Pair a (Pair c (Pair b d)) in Right (run "swapd",    ty)
compileOp a                            Intro1   = let ty = Pair a Unit                in Right (run "intro1",   ty)
compileOp (Pair a Unit)                Elim1    = let ty = a                          in Right (run "elim1",    ty)
compileOp (Pair x e)                   Drop     = let ty = e                          in Right (run "drop",     ty)
compileOp (Pair x e)                   Copy     = let ty = (Pair x (Pair x e))        in Right (run "copy",     ty)
compileOp (Pair Num (Pair Num e))      Add      = let ty = Pair Num e                 in Right (run "add",      ty)
compileOp (Pair Num (Pair Num e))      Multiply = let ty = Pair Num e                 in Right (run "multiply", ty)
compileOp ty op = Left $ "ERROR: type " ++ show ty ++ " does not match operator " ++ show op

compile :: [Op] -> Either String String
compile ops = case compile' (Pair (Pair Num Unit) Unit) ops of
                  Right (compiled, Pair (Pair Num Unit) Unit) -> Right (header ++ compiled ++ footer)
                  Right (compiled, ty) -> Left $ "\n\tOutput type: " ++ show ty ++ "\n\tCompiled:\n" ++ compiled
                  Left err -> Left err
  where
    compile' ty (op:ops) = do
        (compiled, outTy) <- compileOp ty op
        (rest, outTy') <- compile' outTy ops
        return (compiled ++ rest, outTy')
    compile' ty [] = return ("", ty)
