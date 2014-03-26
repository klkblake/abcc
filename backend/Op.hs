module Op where

data Op = AssocL
        | AssocR
        | Swap
        | SwapD
        | Intro1
        | Elim1
        | Drop
        | Copy
        | Add
        | Multiply
        deriving Show
