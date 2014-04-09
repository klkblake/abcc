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
parseOp '+' = Add
parseOp '*' = Multiply

parse ps ('k':cs) = parse ps cs
parse ps ('f':cs) = parse ps cs
parse (p:ps)   ('[':cs) = parse ([]:p:ps) cs
parse (p:q:ps) (']':cs) = parse ((q ++ [LitBlock p]):ps) cs
parse (p:ps)   (c:cs) = parse ((p ++ [parseOp c]):ps) cs
parse [p]      [] = p

--input = concat [dup_abc, dup_abc, mult_abc, add_abc]

-- dup quote dup apply swap apply * +
input = parse [[]] "r^wzlw'[l]o^wzlwvr$crwrwzwlwvr$crwrzw*wrzw+l"

main :: IO ()
main = case compile input of
           Left  err  -> do hPutStrLn stderr $ "Failure: " ++ err
                            exitFailure
           Right prog -> putStr prog
