module Type where

data Type = Pair Type Type
          | Block Type Type
          | Num
          | Unit
          deriving (Show, Eq)
