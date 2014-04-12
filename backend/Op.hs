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
        | IntroNum
        | Digit Int
        | Add
        | Multiply
        | Inverse
        | Negate
        | Divmod
        deriving Show
