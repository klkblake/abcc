module Main where

import Op
import Codegen

add_abc = [AssocR, Swap, AssocR, SwapD, Swap, Add, AssocL]
mult_abc = [AssocR, Swap, AssocR, SwapD, Swap, Multiply, AssocL]
dup_abc = [AssocR, Copy, Swap, SwapD, AssocL, Swap, AssocL]
drop_abc = [AssocR, Drop]

input = dup_abc ++ dup_abc ++ mult_abc ++ add_abc

main :: IO ()
main = putStrLn . either ("FAILURE: " ++) ("SUCCESS: " ++) . compile $ input
