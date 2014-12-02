module PeepholePre
    ( peepholePre
    ) where

import Op

-- R3RIGHT = rwz^QF
-- R3LEFT  = lwz$o+*Q?D>
--
--          l r             w w
--         lw wr              zz
-- <R3RIGHT>w w               ?
--          w w<R3LEFT>       ?
--       wlzr w           lzr
--         zw z               wzw
--          w zz            z zw
--          z zz            z

r3Right :: [FlatOp]
r3Right = [ AssocR
          , Swap
          , SwapD
          , Copy
          , Divmod
          , Factor
          ]

r3Left :: [FlatOp]
r3Left = [ AssocL
         , Swap
         , SwapD
         , Apply
         , Compose
         , Add
         , Multiply
         , Divmod
         , CondApply
         , Distrib
         , Greater
         ]

simplify :: [RawOp] -> [RawOp] -> [RawOp]
simplify l (LitBlock ops:r) = simplify (LitBlock (peepholePre ops):l) r
simplify (Op AssocL:l) (Op AssocR:r) = simplify (Op Swap:l) (Op Swap:r)
simplify (Op Swap:Op AssocL:l) (Op Swap:Op AssocR:r) = simplify l (Op SwapD:Op SwapD:r)
simplify (Op Swap:Op x:l) (Op Swap:r) | x `elem` r3Right = simplify l (Op x:r)
simplify (Op Swap:l) (Op Swap:Op x:r) | x `elem` r3Left = simplify l (Op x:r)
simplify (Op AssocR:Op SwapD:Op AssocL:Op Swap:l) (Op Swap:r) = simplify (Op AssocR:Op SwapD:Op AssocL:l) r
simplify (Op Swap:Op SwapD:l) (Op SwapD:r) = simplify l (Op Swap:Op SwapD:Op Swap:r)
simplify (Op Swap:l) (Op SwapD:Op SwapD:r) = simplify (Op SwapD:l) (Op SwapD:Op Swap:r)
simplify (Op SwapD:l) (Op SwapD:Op SwapD:r) = simplify (Op SwapD:l) r
simplify l (x:r) = simplify (x:l) r
simplify l [] = l

peepholePre :: [RawOp] -> [RawOp]
peepholePre = reverse . simplify []
