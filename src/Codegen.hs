module Codegen where

import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Lazy as Map
import Numeric

import Data.ReinterpretCast

import Op

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

emitBlock :: [RawOp] -> Codegen Int
emitBlock b = do
    CodegenState bs ts c <- get
    let n = Map.size bs
    put $ CodegenState (Map.insert n [] bs) ts n
    -- Detect tail calls
    case reverse b of
        (Op Elim1:Op Apply:b') -> mapM_ compileOp . reverse $ Op ApplyTail:b'
        _ -> mapM_ compileOp b
    modify $ \(CodegenState bs' ts' _) -> CodegenState bs' ts' c
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
                    insertTextNode n "" "" ""
                    (_, tref) <- emitText cs
                    -- Ideally, we'd be ORing in the sumLeft/sumRight
                    -- values, but GAS only permits addition when the
                    -- operands are in different sections, e.g. .rodata and
                    -- *ABS*.
                    let ref = quad $ "(text + " ++ show n ++ "*8) + " ++ show sumLeft
                    let text = quad $ "_text_nodes + " ++ show n ++ "*16"
                    -- Remember, all numbers are stored inverted
                    insertTextNode n ref text $ quad ("0x" ++ showHex (complement $ doubleToWord (fromIntegral $ ord c :: Double)) "") ++ tref
                    return (n, ref)
                [] -> do
                    let ref = quad $ "Unit + " ++ show sumRight
                    let text = quad unit
                    insertTextNode n ref text $ text ++ text
                    return (n, ref)
  where
    -- Must match rts.h
    sumLeft = 0 :: Int
    sumRight = 1 :: Int
    unit = "_Unit"
    line x = "\"\t " ++ x ++ "\\n\"\n"
    quad x = line $ ".quad " ++ x
    insertTextNode n ref text textNode =
        modify $ \(CodegenState bs ts c) -> CodegenState bs (Map.insert t (n, (ref, (text, textNode))) ts) c

compileOp :: RawOp -> Codegen ()
compileOp (LitBlock b) = emitBlock b >>= \n -> emit $ "\tv = pair((Any) &block_" ++ show n ++ ", v);\n"
compileOp (Op op) = go op
  where
    go (LitText  t) = emitText  t >>= \(n, _) -> emit $ "\tv = pair(TAG((Any) &text[" ++ show n ++ "], SUM_LEFT), v);\n"
    go AssocL         = emitOp "assocl"
    go AssocR         = emitOp "assocr"
    go Swap           = emitOp "swap"
    go SwapD          = emitOp "swapd"
    go Intro1         = emitOp "intro1"
    go Elim1          = emitOp "elim1"
    go Drop           = emitOp "drop"
    go Copy           = emitOp "copy"
    go Apply          = emitOp "apply"
    go ApplyTail      = emitOp "apply_tail"
    go Compose        = emitOp "compose"
    go Quote          = emitOp "quote"
    go Relevant       = return ()
    go Affine         = return ()
    go IntroNum       = emitOp "introNum"
    go (Digit d)      = emit $ "\tv = digit(" ++ show d ++ ", v);\n"
    go Add            = emitOp "add"
    go Multiply       = emitOp "multiply"
    go Inverse        = emitOp "inverse"
    go Negate         = emitOp "negate"
    go Divmod         = emitOp "divmod"
    go AssocLS        = emitOp "assocls"
    go AssocRS        = emitOp "assocrs"
    go SwapS          = emitOp "swaps"
    go SwapDS         = emitOp "swapds"
    go Intro0         = emitOp "intro0"
    go Elim0          = emitOp "elim0"
    go CondApply      = emitOp "condapply"
    go Distrib        = emitOp "distrib"
    go Factor         = emitOp "factor"
    go Merge          = emitOp "merge"
    go Assert         = emitOp "assert"
    go Greater        = emitOp "greater"
    go (Sealer   _)   = return ()
    go (Unsealer _)   = return ()
    go AssertEQ       = emitOp "assert_eq"
    go DebugPrintRaw  = emitOp "debug_print_raw"
    go DebugPrintText = emitOp "debug_print_text"

genC :: Map.Map Int String -> Map.Map String (Int, (String, (String, String))) -> String
genC bs ts = let ts' = map (snd . snd) . sortFst . map snd . Map.toList $ ts
             in "#include \"rts.h\"\n\nextern const Any text[];\nextern const struct pair text_nodes[];\n\n" ++ genText (map fst ts') ++ "\n" ++ genTextNodes (map snd ts') ++ "\n" ++ "\n" ++ concatMap (uncurry genBlocks) (reverse . sortFst . Map.toList $ bs)
  where
    sortFst = sortBy (compare `on` fst)
    genText ts' = "__asm__(\n\".pushsection .rodata\\n\"\n\".align 4\\n\"\n\"text:\\n\"\n" ++ concat ts' ++ "\n"
    genTextNodes ts' = "\"_text_nodes:\\n\"\n" ++ concat ts' ++ "\".popsection\\n\"\n);\n"
    genBlocks n b = "\nAny block_" ++ show n ++ "(Any v) {\n" ++ b ++ "\treturn v;\n}\n"

compile :: [RawOp] -> Either String String
compile ops = case runStateT (emitBlock ops) $ CodegenState Map.empty Map.empty 0  of
                  Right (_, cgs) -> Right $ genC (blocks cgs) (textNodes cgs)
                  Left err -> Left err
