{-# LANGUAGE TypeFamilies #-}
module Jaffar where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Sequence (Seq, (|>), viewl, ViewL (..))
import qualified Data.Sequence as S
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Numeric

type ID = Int

data Node = Node String ID String

instance Show Node where
    showsPrec _ (Node prefix ident label) = showString prefix . showInt ident . showString " [label=\"" . showString label . showString "]"

data Edge = Edge String ID ID String

instance Show Edge where
    showsPrec _ (Edge prefix from to label) = showID from . showString " -> " . showID to . showString " [label=\"" . showString label . showString "]"
      where
        showID ident = showString prefix . showInt ident

class Graph g where
    type State g
    uniqueID :: g -> ID
    toGraph :: String -> STRef (State g) IntSet -> g -> ST (State g) ([Node], [Edge])

doIfUnseen :: STRef s IntSet -> ID -> a -> ST s a -> ST s (a)
doIfUnseen seenRef ident x a = do
    seen <- readSTRef seenRef
    if IS.member ident seen
        then return x
        else writeSTRef seenRef (IS.insert ident seen) >> a

showSubgraph :: Graph g => String -> g -> ST (State g) String
showSubgraph name g = do
    seen <- newSTRef IS.empty
    graph <- toGraph name seen g
    return . shows "subgraph {\n" . shows graph . shows "\n}" $ ""

data TaggedTreeList s = TTLNode (TaggedTreeList s) (TaggedTreeList s) Int Int
                      | TTLLeaf (TermNode s) Int
                      | TTLEmpty

singleton :: TermNode s -> TaggedTreeList s
singleton t@(TermNode _ _ Nothing  _ _) = TTLLeaf t 1
singleton t@(TermNode _ _ (Just _) _ _) = TTLLeaf t 0

size :: TaggedTreeList s -> Int
size (TTLNode _ _ s _) = s
size (TTLLeaf _ _) = 1
size TTLEmpty = 0

termCount :: TaggedTreeList s -> Int
termCount (TTLNode _ _ _ c) = c
termCount (TTLLeaf _ c) = c
termCount TTLEmpty = 0

ttlConcat :: TaggedTreeList s -> TaggedTreeList s -> TaggedTreeList s
ttlConcat TTLEmpty r = r
ttlConcat l TTLEmpty = l
ttlConcat l r = TTLNode l r (size l + size r) (termCount l + termCount r)

ttlCons :: TermNode s -> TaggedTreeList s -> TaggedTreeList s
ttlCons h t = singleton h `ttlConcat` t

ttlToVector :: TaggedTreeList s -> ST s (MVector s (TermNode s))
ttlToVector ttl = MV.new (size ttl) >>= \v -> go v ttl >> return v
  where
    go v (TTLNode l r s _) = let s' = size l
                             in go (MV.slice 0 s' v) l >> go (MV.slice s' (s - s') v) r
    go v (TTLLeaf t _) = MV.write v 0 t
    go _ TTLEmpty = return ()

data Symbol = Symbol String Int

instance Show Symbol where
    show (Symbol name arity) = name ++ '/':show arity

data TermNode s = TermNode ID Symbol (Maybe (VarNode s)) (MVector s (TermNode s)) (STRef s Bool)

instance Graph (TermNode s) where
    type State (TermNode s) = s
    uniqueID (TermNode ident _ _ _ _) = ident
    toGraph prefix seenRef (TermNode ident sym var children doneRef) = doIfUnseen seenRef ident ([], []) $ do
        let label = case var of
                        Just (VarNode _ sym' _ _ _) -> sym'
                        Nothing -> show sym
        done <- readSTRef doneRef
        let check = if done
                        then ""
                        else " ✓"
            node = Node prefix ident $ label ++ check
            edge = Edge prefix ident
        children' <- V.toList <$> V.freeze children
        let edges = zipWith (\c i -> edge (uniqueID c) $ '#':show i) children' [1 :: Int ..]
        (vns, ves) <- case var of
                          Just v  -> (\(x, y) -> (x, edge (uniqueID v) "var":y)) <$> toGraph prefix seenRef v
                          Nothing -> return ([], [])
        (cns, ces) <- unzip <$> mapM (toGraph prefix seenRef) children'
        return (node:vns ++ concat cns, edges ++ ves ++ concat ces)

substitute :: TermNode s -> ST s ()
substitute (TermNode _ _ _ children doneRef) = do
    done <- readSTRef doneRef
    if done
        then return ()
        else do
            writeSTRef doneRef True
            V.sequence_ . V.imap go =<< V.freeze children
  where
    go _ c@(TermNode _ _ Nothing _ _) = substitute c
    go i (TermNode _ _ (Just var) _ _) = do
        VarNode _ _ _ terms _ <- rep var
        ts <- ttlToVector =<< readSTRef terms
        when (MV.length ts /= 1) $ error "Attempted to substitute variable with multiple terms"
        MV.write children i =<< MV.read ts 0

data VarNode s = VarNode ID String (STRef s (Maybe (VarNode s))) (STRef s (TaggedTreeList s)) (STRef s Int)

instance Graph (VarNode s) where
    type State (VarNode s) = s
    uniqueID (VarNode ident _ _ _ _) = ident
    toGraph prefix seenRef (VarNode ident sym repRef terms varCountRef) = doIfUnseen seenRef ident ([], []) $ do
        varCount <- readSTRef varCountRef
        let node = Node prefix ident $ sym ++ " (" ++ show varCount ++ ")"
        rep' <- readSTRef repRef
        let edge = Edge prefix ident
        (rns, res) <- case rep' of
                          Just r -> (\(x, y) -> (x, edge (uniqueID r) "rep":y)) <$> toGraph prefix seenRef r
                          Nothing -> return ([], [])
        ts <- V.toList <$> (V.freeze =<< ttlToVector =<< readSTRef terms)
        let edges = zipWith (\c i -> edge (uniqueID c) $ '#':show i) ts [1 :: Int ..]
        (cns, ces) <- unzip <$> mapM (toGraph prefix seenRef) ts
        return (node:rns ++ concat cns, edges ++ res ++ concat ces)

add :: Seq (VarNode s) -> VarNode s -> TermNode s -> ST s (Seq (VarNode s))
add queue v@(VarNode _ _ _ termsRef _) t = do
    ts <- readSTRef termsRef
    writeSTRef termsRef $ ttlCons t ts
    return $ if termCount ts == 1
                 then queue |> v
                 else queue

merge :: Seq (VarNode s) -> VarNode s -> VarNode s -> ST s (Seq (VarNode s))
merge queue v1@(VarNode _ _ _ _ varCountRef1) v2@(VarNode _ _ repRef _ varCountRef2) = do
    r1 <- readSTRef varCountRef1
    r2 <- readSTRef varCountRef2
    let vc = r1 + r2
    if r1 >= r2
        then go vc v1 v2
        else go vc v2 v1
  where
    go vc bigV@(VarNode _ _ _ bigTermsRef bigVarCountRef) (VarNode _ _ _ termsRef varCountRef) = do
        bigTerms <- readSTRef bigTermsRef
        terms    <- readSTRef termsRef
        let bigTerms' = case bigTerms of
                            TTLEmpty -> terms
                            _     -> bigTerms `ttlConcat` terms
        writeSTRef bigTermsRef bigTerms'
        writeSTRef repRef (Just bigV)
        writeSTRef termsRef TTLEmpty
        writeSTRef varCountRef 0
        writeSTRef bigVarCountRef vc
        return $ if termCount bigTerms <= 1 && termCount bigTerms' >= 2
                     then queue |> bigV
                     else queue

rep :: VarNode s -> ST s (VarNode s)
rep v = do
    r <- findRep v
    setRep r v
  where
    findRep v'@(VarNode _ _ repRef _ _) = do
        r <- readSTRef repRef
        case r of
            Just r' -> findRep r'
            Nothing -> return v'
    setRep r (VarNode _ _ repRef _ _) = do
        r' <- readSTRef repRef
        case r' of
            Just r'' -> writeSTRef repRef (Just r) >> setRep r r''
            Nothing  -> return r

{-

func commonFrontier(queue []*VarNode, index int, parents, t_list []*TermNode) ([]*VarNode, *UnificationError) {
	sym := t_list[0].symbol
	for _, term := range t_list {
		if term.symbol != sym {
			return nil, &UnificationError{
				left:  sym,
				right: term.symbol,
			}
		}
	}
	a := sym.arity
	t0_list := make([]*TermNode, len(t_list))
	for i := 0; i < a; i++ {
		for j := range t_list {
			t0_list[j] = t_list[j].child[i]
		}
		j := -1
		for k, term := range t0_list {
			if term.varNode != nil {
				j = k
				break
			}
		}
		if j != -1 {
			// In the original this unconditionally swaps them in memory
			if parents != nil {
				tmp := parents[0].child[index]
				parents[0].child[index] = parents[j].child[index]
				parents[j].child[index] = tmp
			}
			v := rep(t0_list[j].varNode)
			for k, term := range t0_list {
				if k == j || term.varNode == nil {
					continue
				}
				v2 := rep(term.varNode)
				if v != v2 {
					queue = merge(queue, v, v2)
				}
			}
			for _, term := range t0_list {
				if term.varNode != nil {
					continue
				}
				queue = add(queue, v, term)
			}
		} else {
			return commonFrontier(queue, i, t_list, t0_list)
		}
	}
	return queue, nil
}

func unify(t_list []*TermNode) *UnificationError {
	queue := make([]*VarNode, 0)
	queue, err := commonFrontier(queue, 0, nil, t_list)
	if err != nil {
		return err
	}
	for len(queue) != 0 {
		v := queue[0]
		queue = queue[1:]
		k := v.terms.TermCount()
		if k >= 2 {
			t := v.terms.Slice()
			v.terms = NewTaggedTreeList(t[0])
			queue, err = commonFrontier(queue, 0, nil, t)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func main() {
	sym := func(name string, arity int) *Symbol {
		return &Symbol{ name, arity }
	}
	varNode := func(name string) *VarNode {
		return &VarNode{ name, nil, nil, 1 }
	}
	varTerm := func(node *VarNode) *TermNode {
		return &TermNode{ nil, node, nil, false }
	}
	funcTerm := func(symbol *Symbol, children ...*TermNode) *TermNode {
		return &TermNode{ symbol, nil, children, false }
	}
	f := sym("f", 2)
	x := varTerm(varNode("x"))
	z := varTerm(varNode("z"))
	expr1 := funcTerm(f, x, x)
	expr2 := funcTerm(f, funcTerm(f, z, z), funcTerm(f, z, x))
	root := NewTaggedTreeList(expr1).Append(expr2)
	fmt.Println("digraph {")
	root.PrintCyclicRoot("initial")
	err := unify(root.Slice())
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
	root.PrintCyclicRoot("unify")
	expr1.Substitute()
	expr2.Substitute()
	root.PrintCyclicRoot("substitute")
	fmt.Println("}")
}

-}
