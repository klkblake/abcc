{-# LANGUAGE PatternSynonyms #-}
module Expansion where

import Control.Applicative hiding (empty)
import Control.Monad.State

import qualified InterList as IL
import Type
import qualified Op as O

data UOp = ConstBlock Graph
         | UOp FlatUOp
         deriving Show

data FlatUOp = CreatePair
             | DestroyPair
             | ConstText String
             | Intro1
             | Elim1
             | Drop
             | Copy
             | Apply
             | Compose
             | Quote
             | ConstNumber Rational
             | Add
             | Multiply
             | Inverse
             | Negate
             | Divmod
             | CreateSum
             | DestroySum
             | Intro0
             | Elim0
             | CondApply
             | Distrib
             | Merge
             | Assert
             | Greater
             | AssertEQ
             | DebugPrintRaw
             | DebugPrintText
             deriving Show

{- 
Notes on graph design:
 - Implicit ordering on output links
 - Nodes that have in-degree > 1 have output links positioned at
   location of lowest input link (measured on far side). This should
   hopefully remove the need for swapping output links.
 - Nodes that have in-degree = 0 are positioned at the bottom
-}

{-
type Slot = Int
type Dist = Int
type Port = (ID, Slot)

data LinkForward = LinkForward ID Slot

data Link = Link ID Slot Dist Type

data HalfLink = Link ID Slot Dist Type
-}

type ID   = Int
type Slot = Int
type Dist = Int

data Link = Link !Dist ID Slot Type
          | StartLink ID Type
          deriving Show

instance Eq Link where
     Link _ ident slot _ == Link _ ident' slot' _ = ident == ident' && slot == slot'
     StartLink ident _ == StartLink ident' _ = ident == ident'
     _ == _ = False

data Node = Node ID UOp [Link]
          deriving Show

data ParGroup = ParGroup [Node]
              deriving Show

data Graph = Graph !ID [Link] [ParGroup] [Link]
           deriving Show

empty :: [Type] -> Graph
empty tys =
    let links = zipWith StartLink [0..] tys
    in Graph (length tys) links [] links

linksE :: Graph -> [Link]
linksE (Graph _ _ _ ls) = ls

swapE :: Link -> Link -> Graph -> Graph
swapE a b (Graph nextID lsS pgs lsE) = Graph nextID lsS pgs $ swap lsE
  where
    swap (l:ls) | l == a = b:replace b a ls
                | l == b = a:replace a b ls
                | otherwise = l:swap ls
    swap [] = error $ "Links " ++ show a ++ " and " ++ show b ++ " are not at the end of the graph"
    replace a' b' (l:ls) | l == a' = b':ls
                         | otherwise = l:replace a' b' ls
    replace a' _ [] = error $ "Link " ++ show a' ++ " is not at the end of the graph"

swapEM :: Link -> Link -> State Graph ()
swapEM a b = modify $ swapE a b

snoc :: UOp -> [Link] -> [Type] -> Graph -> ([Link], Graph)
snoc uop links outTys (Graph nextID lsS pgs lsE) =
    let oldLinks = map incDist $ filter (not . flip elem links) lsE
        newLinks = zipWith (Link 0 nextID) [0..] outTys
        n = Node nextID uop links
    in (newLinks, Graph (nextID + 1) lsS (ParGroup [n]:pgs) $ oldLinks ++ newLinks)
  where
    incDist (Link dist ident slot ty) = Link (dist + 1) ident slot ty
    incDist (StartLink ident ty) = StartLink ident ty

snocM :: UOp -> [Link] -> [Type] -> State Graph [Link]
snocM uop links outTys = state $ snoc uop links outTys

snocM_ :: UOp -> [Link] -> [Type] -> State Graph ()
snocM_ uop links outTys = void $ snocM uop links outTys

snocFM :: FlatUOp -> [Link] -> [Type] -> State Graph [Link]
snocFM uop = snocM (UOp uop)

snocFM_ :: FlatUOp -> [Link] -> [Type] -> State Graph ()
snocFM_ uop links outTys = void $ snocFM uop links outTys

endList :: Int -> Type -> State Graph [Link]
endList n ty = do
    ls <- gets linksE
    let len = length ls
    case compare n len of
        EQ -> return ls
        LT -> (take (n   - 1) ls ++) <$> goLT           (dropTy (n   - 1) ty) (drop (n - 1) ls)
        GT -> (take (len - 1) ls ++) <$> goGT (n - len) (dropTy (len - 1) ty) (last ls)
  where
    dropTy 0 ty' = ty'
    dropTy n' (_ :*: ty') = dropTy (n' - 1) ty'
    dropTy _ ty' = error $ "dropTy: Expected pair, got " ++ show ty'
    goLT _ [a] = return [a]
    goLT ty'@(_ :*: ty'') (a:ls) = do
        [b] <- goLT ty'' ls
        snocFM CreatePair [a, b] [ty']
    goLT ty' _ = error $ "goLT: Expected pair, got " ++ show ty'
    goGT 0  _ l = return [l]
    goGT n' (ta :*: tb) l = do
        [a, b] <- snocFM DestroyPair [l] [ta, tb]
        (a:) <$> goGT (n' - 1) tb b
    goGT _ ty' _ = error $ "goGT: Expected pair, got " ++ show ty'

addFlatOp :: O.FlatOp -> Type -> Type -> State Graph ()
addFlatOp O.AssocL ty (tab :*: _) = do
    [a, b, _] <- endList 3 ty
    snocFM_ CreatePair [a, b] [tab]

addFlatOp O.AssocR ty (ta :*: (tb :*: _)) = do
    [a, _] <- endList 2 ty
    snocFM_ DestroyPair [a] [ta, tb]

addFlatOp O.Swap ty (_ :*: (_ :*: _)) = do
    [a, b, _] <- endList 3 ty
    swapEM a b

addFlatOp O.SwapD ty (_ :*: (_ :*: (_ :*: _))) = do
    [_, b, c, _] <- endList 4 ty
    swapEM b c

addFlatOp O.Intro1 ty (_ :*: t1) = do
    _ <- endList 1 ty
    snocFM_ Intro1 [] [t1]

addFlatOp O.Elim1 ty ta = do
    [_, b] <- endList 2 ty
    snocFM_ Elim1 [b] [ta]

addFlatOp O.Drop ty te = do
    [a, _] <- endList 2 ty
    snocFM_ Drop [a] [te]

addFlatOp O.Copy ty (tx1 :*: (tx2 :*: _)) = do
    [a, _] <- endList 2 ty
    snocFM_ Copy [a] [tx1, tx2]

addFlatOp op tyS tyE = error $ show op ++ ", " ++ show tyS ++ ", " ++ show tyE

addOp :: O.TyOp -> Type -> Type -> State Graph ()
addOp (O.Op op) = addFlatOp op

expand :: IL.InterList Type O.TyOp -> Graph
expand tyOps = execState addAll $ empty [head $ IL.outerList tyOps]
  where
    outer = IL.outerList tyOps
    addAll = sequence . zipWith3 addOp (IL.innerList tyOps) outer $ tail outer