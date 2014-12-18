{-# LANGUAGE PatternSynonyms, BangPatterns #-}
module Expansion where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Foldable (foldMap)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import qualified GraphViz as GV
import qualified InterList as IL
import Type
import qualified Op as O

data UOp = ConstBlock Graph
         | UOp FlatUOp
         deriving Show

data FlatUOp = ConstList [Rational]
             | CreatePair
             | DestroyPair
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
             | Seal String
             | Unseal String
             | AssertEQ
             | DebugPrintRaw
             | DebugPrintText
             deriving (Show, Eq, Ord)

arity :: UOp -> (Int, Int)
arity (ConstBlock _) = (0, 1)
arity (UOp uop) = arity' uop
  where
    arity' (ConstList _)   = (0, 1)
    arity' CreatePair      = (2, 1)
    arity' DestroyPair     = (1, 2)
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
    arity' CondApply       = (2, 1)
    arity' Distrib         = (1, 2)
    arity' Merge           = (2, 1)
    arity' Assert          = (2, 1)
    arity' Greater         = (2, 4)
    arity' (Seal _)        = (1, 1)
    arity' (Unseal _)      = (1, 1)
    arity' AssertEQ        = (2, 2)
    arity' DebugPrintRaw   = (1, 1)
    arity' DebugPrintText  = (1, 1)

{- 
Notes on graph design:
 - Implicit ordering on output links
 - Nodes that have in-degree > 1 have output links positioned at
   location of lowest input link (measured on far side). This should
   hopefully reduce the need for swapping output links.
 - Nodes that have in-degree = 0 are positioned at the top
-}

type ID   = Int
type Slot = Int
type Index = Int

-- Index is a hint. The node may be at a lower (but not higher!) index
data Link = Link !Index ID Slot Type
          | StartLink Slot Type
          deriving Show

instance Eq Link where
     Link _ ident slot _ == Link _ ident' slot' _ = ident == ident' && slot == slot'
     StartLink slot _ == StartLink slot' _ = slot == slot'
     _ == _ = False

data Node = Node ID UOp [Link]
          deriving Show

type ParGroup = [Node]

data Graph = Graph !ID !Int [ParGroup] [Link]
           deriving Show

toGraphViz :: String -> Graph -> GV.Graph
toGraphViz prefix g@(Graph _ _ pgs lsE) =
    let start = GV.Node 0 "START"
        end = GV.Node 1 "END"
        endEdges = zipWith endEdge [0 :: Int ..] lsE
        (graphs, nodes, edges) = foldMap nodeNetlist $ concat pgs
    in GV.Graph prefix "" graphs (start:end:nodes) $ endEdges ++ edges
  where
    mkEdge ident ident' label = GV.Edge ident ident' Nothing Nothing label
    endEdge slot2 link@(Link _ ident1 slot1 ty) =
        let Node _ uop1 _ = fst . fromJust $ followLink g link
            port1 = Just $ portForUOpS uop1 slot1
        in GV.Edge (ident1 + 2) 1 port1 Nothing $ show ty ++ "\\n" ++ show slot2
    endEdge slot2 (StartLink slot1 ty) = mkEdge 0 1 $ show slot1 ++ "\\n" ++ show ty ++ "\\n" ++ show slot2
    nodeNetlist (Node ident (ConstBlock g2) []) =
        ( [toGraphViz (prefix ++ "_block_" ++ show ident ++ "_") g2]
        , [GV.Node (ident + 2) $ "ConstBlock " ++ show ident]
        , []
        )
    nodeNetlist (Node ident (UOp uop) ls) =
        ( []
        , [GV.Node (ident + 2) $ show uop]
        , zipWith (edge (UOp uop) ident) [0 :: Int ..] ls
        )
    nodeNetlist (Node ident (ConstBlock _) ls) = error $ "ConstBlock node " ++ show ident ++ " has links: " ++ show ls
    edge uop2 ident2 slot2 link =
        let port2 = Just $ portForUOpE uop2 slot2
            ident2' = ident2 + 2
        in case link of
               Link _ ident slot1 ty ->
                   let Node _ uop1 _ = fst . fromJust $ followLink g link
                       port1 = Just $ portForUOpS uop1 slot1
                   in GV.Edge (ident + 2) ident2' port1 port2 $ show ty
               StartLink slot1 ty -> GV.Edge 0 ident2' Nothing port2 $ show slot1 ++ "\\n" ++ show ty
    portForUOpS uop slot = portForUOpS' (snd $ arity uop) slot
    portForUOpS' 1 0 = GV.S
    portForUOpS' 2 0 = GV.SW
    portForUOpS' 2 1 = GV.SE
    portForUOpS' 3 0 = GV.SW
    portForUOpS' 3 1 = GV.S
    portForUOpS' 3 2 = GV.SE
    portForUOpS' 4 0 = GV.SW
    portForUOpS' 4 1 = GV.S
    portForUOpS' 4 2 = GV.SE
    portForUOpS' 4 3 = GV.E
    portForUOpS' n s = error $ "Invalid port " ++ show s ++ "/" ++ show n
    portForUOpE uop slot = portForUOpE' (fst $ arity uop) slot
    portForUOpE' 1 0 = GV.N
    portForUOpE' 2 0 = GV.NW
    portForUOpE' 2 1 = GV.NE
    portForUOpE' 3 0 = GV.NW
    portForUOpE' 3 1 = GV.N
    portForUOpE' 3 2 = GV.NE
    portForUOpE' 4 0 = GV.W
    portForUOpE' 4 1 = GV.NW
    portForUOpE' 4 2 = GV.N
    portForUOpE' 4 3 = GV.NE
    portForUOpE' n s = error $ "Invalid port " ++ show s ++ "/" ++ show n

empty :: [Type] -> Graph
empty tys =
    let links = zipWith StartLink [0..] tys
    in Graph 0 0 [] links

linksE :: Graph -> [Link]
linksE (Graph _ _ _ ls) = ls

nodeMatches :: ID -> Node -> Bool
nodeMatches ident (Node ident' _ _) = ident == ident'

followLink :: Graph -> Link -> Maybe (Node, Link)
followLink (Graph _ npgs pgs _) link@(Link idx from slot ty) = findNode idx $ drop (npgs - idx - 1) pgs
  where
    findNode !idx' (pg:pgs') = case find (nodeMatches from) pg of
                                   Just n  -> Just (n, Link idx' from slot ty)
                                   Nothing -> findNode (idx' - 1) pgs'
    findNode _ [] = error $ "Invalid link " ++ show link
followLink _ (StartLink _ _) = Nothing

deleteNode :: Link -> Graph -> Graph
deleteNode link@(Link idx ident _ _) g@(Graph nextID npgs pgs lsE) =
    let (doDec, pgs') = go (npgs - idx - 1) pgs
    in if doDec
           then Graph nextID (npgs - 1) pgs' $ map decLink lsE
           else Graph nextID npgs pgs' lsE
  where
    go (-1) pgs' = go 0 pgs'
    go 0 ([Node ident' _ _]:pgs') | ident' == ident = (True, pgs')
    go 0 (pg:pgs') | Just _ <- find (nodeMatches ident) pg = (False, filter (not . nodeMatches ident) pg:pgs')
    go i (pg:pgs')  =
        let (doDec, pgs'') = case i of
                                 0 -> go i pgs'
                                 _ -> go (i - 1) pgs'
        in (doDec, if doDec then map dec pg:pgs'' else pg:pgs'')
    go _ [] = error $ "Invalid link or graph: " ++ show link ++ ", " ++ show g
    dec (Node ident' uop links) = Node ident' uop $ map decLink links
    decLink (Link idx' ident' slot ty) | idx' > idx = Link (idx' - 1) ident' slot ty
    decLink l = l
deleteNode (StartLink _ _) g = g

swapE :: Link -> Link -> Graph -> Graph
swapE a b (Graph nextID npgs pgs lsE) = Graph nextID npgs pgs $ swap lsE
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
snoc uop links outTys (Graph nextID npgs pgs lsE) =
    let newLinks = zipWith (Link npgs nextID) [0..] outTys
        lsE' = processLinks newLinks lsE links
        n = Node nextID uop links
    in (newLinks, Graph (nextID + 1) (npgs + 1) ([n]:pgs) lsE')
  where
    processLinks new ls     []   = new ++ ls
    processLinks new (l:ls) used | l `elem` used = processLinks new ls $ delete l used
                                 | otherwise     = l:processLinks new ls used
    processLinks _   []     used = error $ "Finished processing links with " ++ show used ++ " left over"

splice :: Eq a => a -> [a] -> [a] -> [a]
splice x ys = concatMap go
  where
    go x' | x' == x = ys
          | otherwise = [x']

-- These are in reverse order
singleCancel :: Map.Map FlatUOp FlatUOp
singleCancel = Map.fromList $ [ (DestroyPair, CreatePair)
                              , (DestroySum, CreateSum)
                              , (Elim1, Intro1)
                              , (Elim0, Intro0)
                              ]

functions1 :: Map.Map FlatUOp (Rational -> Rational)
functions1 = Map.fromList $ [ (Inverse, recip)
                            , (Negate,  negate)
                            ]

functions2 :: Map.Map FlatUOp (Rational -> Rational -> Rational)
functions2 = Map.fromList $ [ (Add,      (+))
                            , (Multiply, (*))
                            ]

snoc' :: UOp -> [Link] -> [Type] -> Graph -> ([Link], Graph)
snoc' (UOp uop) [link] _ g@(Graph nextID npgs pgs lsE)
    | Just uop2 <- Map.lookup uop singleCancel
    , Just (Node _ (UOp uop2') links, _) <- followLink g link
    , uop2' == uop2 =
        (links, deleteNode link . Graph nextID npgs pgs $ splice link links lsE)
snoc' (UOp CreatePair) [linkA, linkB] _ g@(Graph nextID npgs pgs lsE)
    | Just (Node ident (UOp DestroyPair) links, _) <- followLink g linkA
    , Just (Node ident' _ _, _) <- followLink g linkB
    , ident == ident' =
        (links, deleteNode linkA . Graph nextID npgs pgs . delete linkA $ splice linkB links lsE)
snoc' (UOp uop) [link] outTys g@(Graph nextID npgs pgs lsE)
    | Just f <- Map.lookup uop functions1
    , Just (Node _ (UOp (ConstNumber a)) _, _) <- followLink g link =
        let g' = deleteNode link . Graph nextID npgs pgs $ delete link lsE
        in snoc (UOp . ConstNumber $ f a) [] outTys g'
snoc' (UOp uop) [linkA, linkB] outTys g@(Graph nextID npgs pgs lsE)
    | Just f <- Map.lookup uop functions2
    , Just (Node _ (UOp (ConstNumber a)) _, _) <- followLink g linkA
    , Just (Node _ (UOp (ConstNumber b)) _, _) <- followLink g linkB =
        let g' = deleteNode linkA . deleteNode linkB . Graph nextID npgs pgs . delete linkA $ delete linkB lsE
        in snoc (UOp . ConstNumber $ f a b) [] outTys g'
snoc' (UOp Quote) [link] outTys@[Type _ _ _ (Block ts (tb :*: _))] g@(Graph nextID npgs pgs lsE)
    | Just (Node _ (ConstBlock g2) _, _) <- followLink g link =
        let g' = deleteNode link . Graph nextID npgs pgs $ delete link lsE
            new = Graph 1 1 [[Node 0 (ConstBlock g2) []]] [Link 0 0 0 tb, StartLink 0 ts]
        in snoc (ConstBlock new) [] outTys g'
snoc' uop links outTys g = snoc uop links outTys g

snocM :: UOp -> [Link] -> [Type] -> State Graph [Link]
snocM uop links outTys = state $ snoc' uop links outTys

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

addSimple :: Type -> Int -> FlatUOp -> [Type] -> State Graph ()
addSimple ty n uop outTys = do
    input <- endList n ty
    snocFM_ uop (init input) outTys

destroySum2 :: Link -> Type -> State Graph (Link, Link, Link)
destroySum2 abc (ta :+: tbc@(tb :+: tc)) = do
    [a, bc] <- snocFM DestroySum [abc] [ta, tbc]
    [b, c]  <- snocFM DestroySum [bc]  [tb, tc]
    return (a, b, c)
destroySum2 _ _ = error "Invalid type"

destroySum3 :: Link -> Type -> State Graph (Link, Link, Link, Link)
destroySum3 abcd tabcd@(_ :+: (_ :+: (tc :+: td))) = do
    (a, b, cd) <- destroySum2 abcd tabcd
    [c, d]  <- snocFM DestroySum [cd]  [tc, td]
    return (a, b, c, d)
destroySum3 _ _ = error "Invalid type"

createSum2 :: Link -> Link -> Link -> Type -> State Graph Link
createSum2 a b c tabc@(_ :+: tbc) = do
    [bc]  <- snocFM CreateSum  [b, c]  [tbc]
    [abc] <- snocFM CreateSum  [a, bc] [tabc]
    return abc
createSum2 _ _ _ _ = error "Invalid type"

createSum3 :: Link -> Link -> Link -> Link -> Type -> State Graph Link
createSum3 a b c d tabcd@(_ :+: tbcd) = do
    bcd <- createSum2 b c d tbcd
    [abcd] <- snocFM CreateSum [a, bcd] [tabcd]
    return abcd
createSum3 _ _ _ _ _ = error "Invalid type"

createSum2_ :: Link -> Link -> Link -> Type -> State Graph ()
createSum3_ :: Link -> Link -> Link -> Link -> Type -> State Graph ()
createSum2_ a b c   ty = void $ createSum2 a b c   ty
createSum3_ a b c d ty = void $ createSum3 a b c d ty

addFlatOp :: O.FlatOp -> Type -> Type -> State Graph ()
addFlatOp (O.LitText text) ty (tt :*: _) = addSimple ty 1 (ConstList $ map (fromIntegral . fromEnum) text) [tt]

addFlatOp O.AssocL ty (tab :*: _)         = addSimple ty 3 CreatePair  [tab]
addFlatOp O.AssocR ty (ta :*: tb :*: _) = addSimple ty 2 DestroyPair [ta, tb]

addFlatOp O.Swap ty (_ :*: _ :*: _) = do
    [a, b, _] <- endList 3 ty
    swapEM a b

addFlatOp O.SwapD ty (_ :*: _ :*: _ :*: _) = do
    [_, b, c, _] <- endList 4 ty
    swapEM b c

addFlatOp O.Intro1 ty (_ :*: t1) = do
    [a] <- endList 1 ty
    [c1] <- snocFM Intro1 [] [t1]
    swapEM a c1

addFlatOp O.Elim1 ty _ = do
    [_, c1] <- endList 2 ty
    snocFM_ Elim1 [c1] []

addFlatOp O.Drop ty _ = addSimple ty 2 Drop []
addFlatOp O.Copy ty (tx1 :*: (tx2 :*: _)) = addSimple ty 2 Copy [tx1, tx2]

addFlatOp O.Apply    ty (tx'  :*: _) = addSimple ty 3 Apply   [tx']
addFlatOp O.Compose  ty (txz  :*: _) = addSimple ty 3 Compose [txz]
addFlatOp O.Quote    ty (tsxs :*: _) = addSimple ty 2 Quote   [tsxs]
addFlatOp O.Relevant _ _ = return ()
addFlatOp O.Affine   _ _ = return ()

addFlatOp O.IntroNum  ty (tn :*: _) = addSimple ty 1 (ConstNumber 0)  [tn]
addFlatOp (O.Digit d) ty (tn :*: _) = do
    [n, _] <- endList 2 ty
    [c10] <- snocFM (ConstNumber 10) [] [tn]
    [n10] <- snocFM Multiply [n, c10] [tn]
    [cd] <- snocFM (ConstNumber $ fromIntegral d) [] [tn]
    snocFM_ Add [n10, cd] [tn]

addFlatOp O.Add      ty (tn :*: _)          = addSimple ty 3 Add      [tn]
addFlatOp O.Multiply ty (tn :*: _)          = addSimple ty 3 Multiply [tn]
addFlatOp O.Inverse  ty (tn :*: _)          = addSimple ty 2 Inverse  [tn]
addFlatOp O.Negate   ty (tn :*: _)          = addSimple ty 2 Negate   [tn]
addFlatOp O.Divmod   ty (tr :*: tq :*: _) = addSimple ty 3 Divmod   [tr, tq]

addFlatOp O.AssocLS ty@(tabc :*: _) (tabc'@(tab :+: _) :*: _) = do
    [abc, _] <- endList 2 ty
    (a, b, c) <- destroySum2 abc tabc
    [ab]    <- snocFM CreateSum  [a, b] [tab]
    snocFM_ CreateSum  [ab, c] [tabc']

addFlatOp O.AssocRS ty@((tab@(ta :+: tb) :+: tc) :*: _) (tabc :*: _) = do
    [abc, _] <- endList 2 ty
    [ab, c] <- snocFM DestroySum [abc]  [tab, tc]
    [a, b]  <- snocFM DestroySum [ab]   [ta, tb]
    createSum2_ a b c tabc

addFlatOp O.SwapS ty@(tabc :*: _) (tbac :*: _) = do
    [abc, _] <- endList 2 ty
    (a, b, c) <- destroySum2 abc tabc
    createSum2_ b a c tbac

addFlatOp O.SwapDS ty@(tabcd :*: _) (tacbd :*: _) = do
    [abc, _] <- endList 2 ty
    (a, b, c, d) <- destroySum3 abc tabcd
    createSum3_ a c b d tacbd

addFlatOp O.Intro0 ty (ta0@(_ :+: t0) :*: _) = do
    [a, _] <- endList 2 ty
    [c0] <- snocFM Intro0 [] [t0]
    snocFM_ CreateSum [a, c0] [ta0]

addFlatOp O.Elim0 ty@((_ :+: t0) :*: _) (ta :*: _) = do
    [a0, _] <- endList 2 ty
    [_, c0] <- snocFM DestroySum [a0] [ta, t0]
    snocFM_ Elim0 [c0] []

addFlatOp O.CondApply tyS@(_ :*: ((tx :+: ty) :*: _)) (tx'y@(tx' :+: _) :*: _) = do
    [xx', xy, _] <- endList 3 tyS
    [x, y] <- snocFM DestroySum [xy]     [tx, ty]
    [x']   <- snocFM CondApply  [xx', x] [tx']
    snocFM_ CreateSum [x', y] [tx'y]

addFlatOp O.Distrib ty (tabac@(tab@(ta1 :*: tb) :+: tac@(ta2 :*: tc)) :*: _) = do
    [a, bc, _] <- endList 3 ty
    [b, c]   <- snocFM DestroySum [bc] [tb, tc]
    [a1, a2] <- snocFM Distrib    [a]  [ta1, ta2]
    [ab] <- snocFM CreatePair [a1, b] [tab]
    [ac] <- snocFM CreatePair [a2, c] [tac]
    snocFM_ CreateSum [ab, ac] [tabac]

addFlatOp O.Factor ty@((tab@(ta :*: tb) :+: tcd@(tc :*: td)) :*: _) (tac :*: tbd :*: _) = do
    [abcd, _] <- endList 2 ty
    [ab, cd] <- snocFM DestroySum  [abcd] [tab, tcd]
    [a, b]   <- snocFM DestroyPair [ab]   [ta, tb]
    [c, d]   <- snocFM DestroyPair [cd]   [tc, td]
    snocFM_ CreateSum [a, c] [tac]
    snocFM_ CreateSum [b, d] [tbd]

addFlatOp O.Merge ty@((ta :+: ta') :*: _) (tb :*: _) = do
    [aa', _] <- endList 2 ty
    [a, a'] <- snocFM DestroySum [aa'] [ta, ta']
    snocFM_ Merge [a, a'] [tb]

addFlatOp O.Assert ty@((ta :+: tb) :*: _) _ = do
    [ab, _] <- endList 2 ty
    [a, b] <- snocFM DestroySum [ab] [ta, tb]
    snocFM_ Assert [a, b] [tb]

addFlatOp O.Greater ty (tyxxy@(tyx@(ty1 :*: tx1) :+: txy@(tx2 :*: ty2)) :*: _) = do
    [x, y, _] <- endList 3 ty
    [y1, x1, x2, y2] <- snocFM Greater [x, y]  [ty1, tx1, tx2, ty2]
    [yx] <- snocFM CreatePair [y1, x1] [tyx]
    [xy] <- snocFM CreatePair [x2, y2] [txy]
    snocFM_ CreateSum [yx, xy] [tyxxy]

addFlatOp (O.Sealer   seal) tyS tyE = do
    [a] <- endList 1 tyS
    snocFM_ (Seal seal) [a] [tyE]
addFlatOp (O.Unsealer seal) tyS tyE = do
    [a] <- endList 1 tyS
    snocFM_ (Unseal seal) [a] [tyE]

addFlatOp O.AssertEQ       ty (ta :*: tb :*: _) = addSimple ty 3 AssertEQ [ta, tb]
addFlatOp O.DebugPrintRaw  ty (ta :*: _)          = addSimple ty 2 DebugPrintRaw  [ta]
addFlatOp O.DebugPrintText ty (ta :*: _)          = addSimple ty 2 DebugPrintText [ta]

addFlatOp op tyS tyE = error $ show op ++ ", " ++ show tyS ++ ", " ++ show tyE

addOp :: O.TyOp -> Type -> Type -> State Graph ()
addOp (O.LitBlock block) ty (tb :*: _) = do
    _ <- endList 1 ty
    snocM_ (ConstBlock $ expand block) [] [tb]
addOp (O.Op op) tyS tyE = addFlatOp op tyS tyE
addOp (O.LitBlock _) _ ty = error $ "LitBlock with incorrect type " ++ show ty

expand :: IL.InterList Type O.TyOp -> Graph
expand tyOps = execState addAll $ empty [head $ IL.outerList tyOps]
  where
    outer = IL.outerList tyOps
    addAll = sequence . zipWith3 addOp (IL.innerList tyOps) outer $ tail outer
