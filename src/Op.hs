{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts,
 UndecidableInstances #-}
module Op where

import Data.Functor.Identity

import Type

data Op f = LitBlock [f (Op f)]
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

deriving instance Show (f (Op f)) => Show (Op f)

data Typed a = Typed Type Type a
             deriving Show

type UntypedOp = Op Identity
type TypedOp = Typed (Op Typed)
