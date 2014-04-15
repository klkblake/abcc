module Op where

data Op = LitBlock [Op]
        | LitText String
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
        | AssocLS
        | AssocRS
        | SwapS
        | SwapDS
        | Intro0
        | Elim0
        | CondApply
        | Distrib
        | Factor
        | Merge
        | Assert
        | Greater
        deriving Show
