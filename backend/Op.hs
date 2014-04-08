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
        | Add
        | Multiply
        deriving Show
