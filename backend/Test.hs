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

parse ps ('k':cs) = parse ps cs
parse ps ('f':cs) = parse ps cs
parse ps (' ':cs) = parse ps cs
parse ps ('\n':cs) = parse ps cs
parse (p:ps)   ('[':cs) = parse ([]:p:ps) cs
parse (p:q:ps) (']':cs) = parse ((q ++ [LitBlock p]):ps) cs
parse (p:ps)   (c:cs) = parse ((p ++ [parseOp c]):ps) cs
parse [p]      [] = p

main :: IO ()
main = do
    input <- getContents
    case compile $ parse [[]] input of
           Left  err  -> do hPutStrLn stderr $ "Failure: " ++ err
                            exitFailure
           Right prog -> putStr prog
