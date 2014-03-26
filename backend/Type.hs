module Type where

data Type = Pair Type Type
          | Num
          | Unit
          deriving Show
