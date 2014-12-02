{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Op
    ( Op (..)
    , FlatOp (..)
    , opcodeMapping
    , RawOp
    , TyOp
    ) where

import Prelude hiding (foldr, concatMap)
import Data.Foldable
import qualified Data.Map as M

import InterList
import Type

data Op f = LitBlock (f (Op f))
          | Op FlatOp

instance (Functor f, Foldable f) => Show (Op f) where
    showsPrec _ (LitBlock ops) = showChar '[' . foldr (.) id (fmap (showsPrec 0) ops) . showChar ']'
    showsPrec _ (Op op) = showsPrec 0 op

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
            deriving (Eq, Ord)

opcodeMapping :: [(Char, FlatOp)]
opcodeMapping = [ ('l', AssocL)
                , ('r', AssocR)
                , ('w', Swap)
                , ('z', SwapD)
                , ('v', Intro1)
                , ('c', Elim1)
                , ('%', Drop)
                , ('^', Copy)
                , ('$', Apply)
                , ('o', Compose)
                , ('\'', Quote)
                , ('k', Relevant)
                , ('f', Affine)
                , ('#', IntroNum)
                , ('0', Digit 0)
                , ('1', Digit 1)
                , ('2', Digit 2)
                , ('3', Digit 3)
                , ('4', Digit 4)
                , ('5', Digit 5)
                , ('6', Digit 6)
                , ('7', Digit 7)
                , ('8', Digit 8)
                , ('9', Digit 9)
                , ('+', Add)
                , ('*', Multiply)
                , ('/', Inverse)
                , ('-', Negate)
                , ('Q', Divmod)
                , ('L', AssocLS)
                , ('R', AssocRS)
                , ('W', SwapS)
                , ('Z', SwapDS)
                , ('V', Intro0)
                , ('C', Elim0)
                , ('?', CondApply)
                , ('D', Distrib)
                , ('F', Factor)
                , ('M', Merge)
                , ('K', Assert)
                , ('>', Greater)
                ]

opcodes :: M.Map FlatOp Char
opcodes = M.fromList $ map (\(code, op) -> (op, code)) opcodeMapping

instance Show FlatOp where
    showsPrec _ (LitText text) = showChar '"' . showString (concatMap pad text) . showString "\n~"
      where
        pad '\n' = "\n "
        pad c    = [c]
    showsPrec _ (Sealer seal) = showString "{:" . showString seal . showChar '}'
    showsPrec _ (Unsealer seal) = showString "{." . showString seal . showChar '}'
    showsPrec _ AssertEQ = showString "{&≡}"
    showsPrec _ DebugPrintRaw = showString "{&debug print raw}"
    showsPrec _ DebugPrintText = showString "{&debug print text}"
    showsPrec _ op = showChar $ opcodes M.! op

type RawOp = Op []

type TyOp = Op (InterList Type)
