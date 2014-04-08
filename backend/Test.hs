module Main where

import System.IO
import System.Exit

import Op
import Codegen

add_abc  = [AssocR, Swap, AssocR, SwapD, Swap, Add, AssocL]
mult_abc = [AssocR, Swap, AssocR, SwapD, Swap, Multiply, AssocL]
dup_abc  = [AssocR, Copy, Swap, SwapD, AssocL, Swap, AssocL]
drop_abc = [AssocR, Drop]

--input = concat [dup_abc, dup_abc, mult_abc, add_abc]
input = [Intro1, LitBlock dup_abc, Apply, LitBlock dup_abc, Apply, LitBlock mult_abc, LitBlock add_abc, Compose, Apply, Elim1]

main :: IO ()
main = case compile input of
           Left  err  -> do hPutStrLn stderr $ "Failure: " ++ err
                            exitFailure
           Right prog -> putStr prog
