{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Op where

import InterList
import Type

data Op f = LitBlock (f (Op f))
          | Op FlatOp

data FlatOp = LitText String
            | AssocL
            | AssocR
            | Swap
            | SwapD
            | Intro1
            | Elim1
            | Drop
            | Copy
            | Apply
            | ApplyTail
            | Compose
            | Quote
            | Relevant
            | Affine
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
            | Sealer String
            | Unsealer String
            | AssertEQ
            | DebugPrintRaw
            | DebugPrintText
            deriving Show

deriving instance Show (f (Op f)) => Show (Op f)

type RawOp = Op []

type TyOp = Op (InterList Type)
