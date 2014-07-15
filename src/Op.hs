{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts,
 UndecidableInstances, CPP #-}
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

#define c(op) castOp (op) = op
castOp :: Op a -> Op b
castOp (LitBlock _) = error "Can't cast LitBlock"
c(LitText text)
c(AssocL)
c(AssocR)
c(Swap)
c(SwapD)
c(Intro1)
c(Elim1)
c(Drop)
c(Copy)
c(Apply)
c(ApplyTail)
c(Compose)
c(Quote)
c(Relevant)
c(Affine)
c(IntroNum)
c(Digit d)
c(Add)
c(Multiply)
c(Inverse)
c(Negate)
c(Divmod)
c(AssocLS)
c(AssocRS)
c(SwapS)
c(SwapDS)
c(Intro0)
c(Elim0)
c(CondApply)
c(Distrib)
c(Factor)
c(Merge)
c(Assert)
c(Greater)
c(Sealer seal)
c(Unsealer seal)
c(AssertEQ)
c(DebugPrintRaw)
c(DebugPrintText)
#undef c

data PartiallyTyped a = PartiallyTyped String String a
                      deriving Show

data Typed a = Typed Type Type a
             deriving Show

type UntypedOp = Op Identity
type PTypedOp = PartiallyTyped (Op PartiallyTyped)
type TypedOp = Typed (Op Typed)
