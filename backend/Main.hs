module Main where

import System.IO
import System.Exit

import Op
import Codegen

add_abc  = [AssocR, Swap, AssocR, SwapD, Swap, Add, AssocL]
mult_abc = [AssocR, Swap, AssocR, SwapD, Swap, Multiply, AssocL]
dup_abc  = [AssocR, Copy, Swap, SwapD, AssocL, Swap, AssocL]
drop_abc = [AssocR, Drop]

parseOp 'l' = AssocL
parseOp 'r' = AssocR
parseOp 'w' = Swap
parseOp 'z' = SwapD
parseOp 'v' = Intro1
parseOp 'c' = Elim1
parseOp '%' = Drop
parseOp '^' = Copy
parseOp '$' = Apply
parseOp 'o' = Compose
parseOp '\'' = Quote
parseOp '#' = IntroNum
parseOp '0' = Digit 0
parseOp '1' = Digit 1
parseOp '2' = Digit 2
parseOp '3' = Digit 3
parseOp '4' = Digit 4
parseOp '5' = Digit 5
parseOp '6' = Digit 6
parseOp '7' = Digit 7
parseOp '8' = Digit 8
parseOp '9' = Digit 9
parseOp '+' = Add
parseOp '*' = Multiply
parseOp '/' = Inverse
parseOp '-' = Negate
parseOp 'Q' = Divmod
parseOp 'L' = AssocLS
parseOp 'R' = AssocRS
parseOp 'W' = SwapS
parseOp 'Z' = SwapDS
parseOp 'V' = Intro0
parseOp 'C' = Elim0
parseOp '?' = CondApply
parseOp 'D' = Distrib
parseOp 'F' = Factor
parseOp 'M' = Merge
parseOp 'K' = Assert
parseOp '>' = Greater
parseOp op = error $ "Unrecognised opcode: " ++ show op

parseCap (':':_) = []
parseCap ('.':_) = []
parseCap "&debug print raw" = [DebugPrintRaw]
parseCap "&debug print text" = [DebugPrintText]
parseCap cap = error $ "Unrecognised capability: " ++ show cap

parseText ('\n':' ':cs) = let (text, cs') = parseText cs
                          in ('\n':text, cs')
parseText ('\n':'~':cs) = ("", cs)
parseText ('\n':c:cs) = error $ "Invalid character " ++ show c ++ " after newline in text literal"
parseText (c:cs) = let (text, cs') = parseText cs
                   in (c:text, cs')

-- parse code or block, accumulating in reverse on left
--   return properly ordered result and remaining text

parse :: [Op] -> String -> ([Op],String)
parse ps ('k':cs) = parse ps cs
parse ps ('f':cs) = parse ps cs
parse ps (' ':cs) = parse ps cs
parse ps ('\n':cs) = parse ps cs
parse ps ('$':'c':']':cs) = (reverse (ApplyTail:ps), cs)
parse ps (']':cs) = (reverse ps, cs)

parse ps ('[':cs) = let (bops, cs') = parse [] cs
                    in parse (LitBlock bops : ps) cs'

parse ps ('{':cs) = parse (parseCap (takeWhile (/= '}') cs) ++ ps) (tail $ dropWhile (/= '}') cs)
parse ps ('"':cs) = let (text, cs') = parseText cs in parse (LitText text : ps) cs'
parse ps (c:cs) = parse (parseOp c : ps) cs
parse ps [] = (reverse ps,[])

main :: IO ()
main = do
    input <- getContents
    case parse [] input of
        (ops, "") ->
            case compile ops of
                Left  err  -> do hPutStrLn stderr $ "Failure: " ++ err
                                 exitFailure
                Right prog -> putStr prog
        (ops, rem) -> do hPutStrLn stderr $ "Failure: Unmatched closing bracket.\nParsed:\n\t" ++ show ops ++ "\nRemaining:\n\t" ++ rem
                         exitFailure
