module Codegen where

import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Lazy as Map
import Numeric

import qualified LLVM.General.AST as LLVM
import LLVM.General.AST hiding (Type, Add)

import Op
import Type

double :: LLVM.Type
double = FloatingPointType 64 IEEE

run op = "\tv = " ++ op ++ "(v);\n"

data CodegenState = CodegenState { blocks       :: Map.Map Int String
                                 , textNodes    :: Map.Map String (Int, (String, (String, String)))
                                 , currentBlock :: Int
                                 }
                  deriving Show

type Codegen = StateT CodegenState (Either String)

emit :: String -> Codegen ()
emit op = modify $ \(CodegenState bs ts c) -> CodegenState (Map.adjust (++ op) c bs) ts c

emitOp :: String -> Codegen ()
emitOp op = emit $ "\tv = " ++ op ++ "(v);\n"

emitBlock :: [Op] -> Codegen Int
emitBlock b = do
    CodegenState bs ts c <- get
    let n = Map.size bs
    put $ CodegenState (Map.insert n [] bs) ts n
    mapM_ compileOp b
    modify $ \(CodegenState bs ts _) -> CodegenState bs ts c
    return n

emitText :: String -> Codegen (Int, String)
emitText t = do
    ts <- gets textNodes
    case Map.lookup t ts of
        Just (nid, (ref, _)) -> return (nid, ref)
        Nothing       -> do
            let n = Map.size ts
            case t of
                (c:cs) -> do
                    -- Allocate this ID
                    insertTextNode t n "" "" ""
                    (tid, tref) <- emitText cs
                    -- XXX check differences between haskell and C char
                    -- literals
                    -- Ideally, we'd be ORing in the sumLeft/sumRight
                    -- values, but GAS only permits addition when the
                    -- operands are in different sections, e.g. .rodata and
                    -- *ABS*.
                    let ref = quad $ "(text + " ++ show n ++ "*8) + " ++ show sumRight
                    let text = quad $ "_text_nodes + " ++ show n ++ "*16"
                    insertTextNode t n ref text $ line (".double " ++ show (fromIntegral $ ord c :: Double)) ++ tref
                    return (n, ref)
                [] -> do
                    let ref = quad $ "Unit + " ++ show sumLeft
                    let text = quad deadbeef
                    insertTextNode t n ref text $ text ++ text
                    return (n, ref)
  where
    -- Must match rts.h
    sumLeft = 0
    sumRight = 1
    deadbeef = "0x" ++ showHex 0xd34db33f ""
    line x = "\"\t " ++ x ++ "\\n\"\n"
    quad x = line $ ".quad " ++ x
    insertTextNode t n ref text textNode =
        modify $ \(CodegenState bs ts c) -> CodegenState bs (Map.insert t (n, (ref, (text, textNode))) ts) c

compileOp :: Op -> Codegen ()
compileOp (LitBlock b) = emitBlock b >>= \n -> emit $ "\tv = pair((Any) &block_" ++ show n ++ ", v);\n"
compileOp (LitText  t) = emitText  t >>= \(n, _) -> emit $ "\tv = pair(TAG((Any) &text[" ++ show n ++ "], SUM_RIGHT), v);\n"
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
compileOp IntroNum     = emitOp "introNum"
compileOp (Digit d)    = emit $ "\tv = digit(" ++ show d ++ ", v);\n"
compileOp Add          = emitOp "add"
compileOp Multiply     = emitOp "multiply"
compileOp Inverse      = emitOp "inverse"
compileOp Negate       = emitOp "negate"
compileOp Divmod       = emitOp "divmod"
compileOp AssocLS      = emitOp "assocls"
compileOp AssocRS      = emitOp "assocrs"
compileOp SwapS        = emitOp "swaps"
compileOp SwapDS       = emitOp "swapds"
compileOp Intro0       = emitOp "intro0"
compileOp Elim0        = emitOp "elim0"
compileOp CondApply    = emitOp "condapply"
compileOp Distrib      = emitOp "distrib"
compileOp Factor       = emitOp "factor"
compileOp Merge        = emitOp "merge"
compileOp Assert       = emitOp "assert"
compileOp Greater      = emitOp "greater"

genC :: Map.Map Int String -> Map.Map String (Int, (String, (String, String))) -> String
genC bs ts = let ts' = map snd . map snd . sortFst . map snd . Map.toList $ ts
             in "#include \"rts.h\"\n\nextern const Any text[];\nextern const struct pair text_nodes[];\n\n" ++ genText (map fst ts') ++ "\n" ++ genTextNodes (map snd ts') ++ "\n" ++ "\n" ++ concatMap (uncurry genBlocks) (reverse . sortFst . Map.toList $ bs)
  where
    sortFst = sortBy (compare `on` fst)
    genText ts' = "__asm__(\n\".pushsection .rodata\\n\"\n\".align 4\\n\"\n\"text:\\n\"\n" ++ concat ts' ++ "\n"
    genTextNodes ts' = "\"_text_nodes:\\n\"\n" ++ concat ts' ++ "\".popsection\\n\"\n);\n"
    genBlocks n b = "\nAny block_" ++ show n ++ "(Any v) {\n" ++ b ++ "\treturn v;\n}\n"

compile :: [Op] -> Either String String
compile ops = case runStateT (emitBlock ops) $ CodegenState Map.empty Map.empty 0  of
                  Right (_, cgs) -> Right $ genC (blocks cgs) (textNodes cgs)
                  Left err -> Left err
