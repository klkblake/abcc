module Codegen where

import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map.Lazy as Map

import qualified LLVM.General.AST as LLVM
import LLVM.General.AST hiding (Type, Add)

import Op
import Type

double :: LLVM.Type
double = FloatingPointType 64 IEEE

run op = "\tv = " ++ op ++ "(v);\n"

data CodegenState = CodegenState { blocks       :: Map.Map Int String
                                 , currentBlock :: Int
                                 }
                  deriving Show

type Codegen = StateT CodegenState (Either String)

emit :: String -> Codegen ()
emit op = modify $ \(CodegenState bs c) -> CodegenState (Map.adjust (++ op) c bs) c

emitOp :: String -> Codegen ()
emitOp op = emit $ "\tv = " ++ op ++ "(v);\n"

emitBlock :: [Op] -> Codegen Int
emitBlock b = do
    CodegenState bs c <- get
    let n = Map.size bs
    put $ CodegenState (Map.insert n [] bs) n
    mapM_ compileOp b
    modify $ \(CodegenState bs _) -> CodegenState bs c
    return n

compileOp :: Op -> Codegen ()
compileOp (LitBlock b) = emitBlock b >>= \n -> emit $ "\tv = pair(&block_" ++ show n ++ ", v);\n"
compileOp AssocL       = emitOp "assocl"
compileOp AssocR       = emitOp "assocr"
compileOp Swap         = emitOp "swap"
compileOp SwapD        = emitOp "swapd"
compileOp Intro1       = emitOp "intro1"
compileOp Elim1        = emitOp "elim1"
compileOp Drop         = emitOp "drop"
compileOp Copy         = emitOp "copy"
compileOp Apply        = emitOp "apply"
compileOp Compose      = emitOp "compose"
compileOp Quote        = emitOp "quote"
compileOp Add          = emitOp "add"
compileOp Multiply     = emitOp "multiply"

genC :: Map.Map Int String -> String
genC bs = "#include \"rts.h\"\n" ++ concatMap (uncurry genC') (revsort $ Map.toList bs)
  where
    revsort = reverse . sortBy (compare `on` fst)
    genC' n b = "\nAny block_" ++ show n ++ "(Any v) {\n" ++ b ++ "\treturn v;\n}\n"

compile :: [Op] -> Either String String
compile ops = case runStateT (emitBlock ops) $ CodegenState Map.empty 0  of
                  Right (_, cgs) -> Right . genC $ blocks cgs
                  Left err -> Left err
