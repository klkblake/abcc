module UnifyTypes where

import Control.Monad.State

import Type
import Op

unifyTypes :: [TypedOp] -> State TypeContext [TypedOp]
unifyTypes ops = return ops
