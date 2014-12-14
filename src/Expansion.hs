{-# LANGUAGE PatternSynonyms #-}
module Expansion where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Foldable (foldMap)
import Data.List

import qualified GraphViz as GV
import qualified InterList as IL
import Type
import qualified Op as O

data UOp = ConstBlock Graph
         | UOp FlatUOp

instance Show UOp where
    showsPrec p (ConstBlock g) = showParen (p > 10) $ showString "ConstBlock " .  showsPrec 11 g
    showsPrec p (UOp uop) = showsPrec p uop

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

arity :: UOp -> (Int, Int)
arity (ConstBlock _) = (0, 1)
arity (UOp uop) = arity' uop
  where
    arity' CreatePair      = (2, 1)
    arity' DestroyPair     = (1, 2)
    arity' (ConstText _)   = (0, 1)
    arity' Intro1          = (0, 1)
    arity' Elim1           = (1, 0)
    arity' Drop            = (1, 0)
    arity' Copy            = (1, 2)
    arity' Apply           = (2, 1)
    arity' Compose         = (2, 1)
    arity' Quote           = (1, 1)
    arity' (ConstNumber _) = (0, 1)
    arity' Add             = (2, 1)
    arity' Multiply        = (2, 1)
    arity' Inverse         = (1, 1)
    arity' Negate          = (1, 1)
    arity' Divmod          = (2, 2)
    arity' CreateSum       = (2, 1)
    arity' DestroySum      = (1, 2)
    arity' Intro0          = (0, 1)
    arity' Elim0           = (1, 0)
    arity' CondApply       = (3, 2)
    arity' Distrib         = (3, 4)
    arity' Merge           = (2, 1)
    arity' Assert          = (2, 1)
    arity' Greater         = (2, 4)
    arity' AssertEQ        = (2, 2)
    arity' DebugPrintRaw   = (1, 0)
    arity' DebugPrintText  = (1, 0)

{- 
Notes on graph design:
 - Implicit ordering on output links
 - Nodes that have in-degree > 1 have output links positioned at
   location of lowest input link (measured on far side). This should
   hopefully remove the need for swapping output links.
 - Nodes that have in-degree = 0 are positioned at the bottom
-}

type ID   = Int
type Slot = Int
type Dist = Int

data Link = Link !Dist ID Slot Type
          | StartLink Slot Type
          deriving Show

instance Eq Link where
     Link _ ident slot _ == Link _ ident' slot' _ = ident == ident' && slot == slot'
     StartLink slot _ == StartLink slot' _ = slot == slot'
     _ == _ = False

data Node = Node ID UOp [Link]
          deriving Show

type ParGroup = [Node]

data Graph = Graph !ID [ParGroup] [Link]
           deriving Show

toGraphViz :: Graph -> GV.Graph
toGraphViz (Graph _ pgs lsE) =
    let start = GV.Node 0 "START"
        end = GV.Node 1 "END"
        endEdges = zipWith endEdge [0 :: Int ..] lsE
        (nodes, edges) = foldMap nodeNetlist $ concat pgs
    in GV.Graph "node" "" [] (start:end:nodes) $ endEdges ++ edges
  where
    mkEdge ident ident' label = GV.Edge ident ident' Nothing Nothing label
    endEdge slot' (Link _ ident slot ty) = mkEdge (ident + 2) 1 $ edgeLabel slot slot' ty
    endEdge slot' (StartLink slot ty) = mkEdge 0 1 $ edgeLabel slot slot' ty
    nodeNetlist (Node ident uop ls) = ([GV.Node (ident + 2) $ show uop], zipWith (edge uop ident) [0 :: Int ..] ls)
    edge uop ident2 slot2 link =
        let port' = Just $ portForUOpE uop slot2
            ident2' = ident2 + 2
        in case link of
               Link _ ident slot1 ty -> GV.Edge (ident + 2) ident2' Nothing port' $ show slot1 ++ " >> " ++ show ty
               StartLink    slot1 ty -> GV.Edge 0           ident2' Nothing port' $ show slot1 ++ " >> " ++ show ty
    portForUOpE uop slot = portForUOpE' (fst $ arity uop) slot
    portForUOpE' 1 0 = GV.N
    portForUOpE' 2 0 = GV.NW
    portForUOpE' 2 1 = GV.NE
    portForUOpE' 3 0 = GV.NW
    portForUOpE' 3 1 = GV.N
    portForUOpE' 3 2 = GV.NE
    portForUOpE' 4 0 = GV.W
    portForUOpE' 4 2 = GV.NW
    portForUOpE' 4 3 = GV.N
    portForUOpE' 4 4 = GV.NE
    portForUOpE' n s = error $ "Invalid port " ++ show s ++ "/" ++ show n
    edgeLabel slot slot' ty = show slot ++ " >> " ++ show ty ++ " >> " ++ show slot'

empty :: [Type] -> Graph
empty tys =
    let links = zipWith StartLink [0..] tys
    in Graph 0 [] links

linksE :: Graph -> [Link]
linksE (Graph _ _ ls) = ls

swapE :: Link -> Link -> Graph -> Graph
swapE a b (Graph nextID pgs lsE) = Graph nextID pgs $ swap lsE
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
snoc uop links outTys (Graph nextID pgs lsE) =
    let newLinks = zipWith (Link 0 nextID) [0..] outTys
        lsE' = processLinks newLinks lsE links
        n = Node nextID uop links
    in (newLinks, Graph (nextID + 1) ([n]:pgs) lsE')
  where
    processLinks new ls     []   = new ++ map incDist ls
    processLinks new (l:ls) used | l `elem` used = processLinks new ls $ delete l used
                                 | otherwise     = incDist l:processLinks new ls used
    processLinks _   []     used = error $ "Finished processing links with " ++ show used ++ " left over"
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
