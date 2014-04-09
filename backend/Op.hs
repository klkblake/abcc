module Op where

data Op = LitBlock [Op]
        | AssocL
        | AssocR
        | Swap
        | SwapD
        | Intro1
        | Elim1
        | Drop
        | Copy
        | Apply
        | Compose
        | Quote
        | Add
        | Multiply
        deriving Show
