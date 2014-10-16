{-# LANGUAGE TypeFamilies #-}
module Main where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Numeric

type ID = Int

data Node = Node String ID String

instance Show Node where
    showsPrec _ (Node prefix ident label) = showString prefix . showChar '_' . showInt ident . showString " [label=\"" . showString label . showString "\"]"

data Edge = Edge String ID ID String

instance Show Edge where
    showsPrec _ (Edge prefix from to label) = showID from . showString " -> " . showID to . showString " [label=\"" . showString label . showString "\"]"
      where
        showID ident = showString prefix . showChar '_' . showInt ident

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
    (nodes, edges) <- toGraph name seen g
    return . showString "subgraph {\n" . showConcat nodes . showChar '\n' . showConcat edges . showString "\n}" $ ""
  where
    showConcat :: Show a => [a] -> ShowS
    showConcat = foldr (.) id . intersperse (showChar '\n') . map shows

data Queue a = Queue [a] [a]

queueEmpty :: Queue a
queueEmpty = Queue [] []

queuePush :: Queue a -> a -> Queue a
queuePush (Queue f b) x = Queue f $ x:b

queuePop :: Queue a -> Maybe (a, Queue a)
queuePop (Queue (x:f) b) = Just (x, Queue f b)
queuePop (Queue [] b@(_:_)) = queuePop $ Queue (reverse b) []
queuePop (Queue [] []) = Nothing

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
            deriving Eq

instance Show Symbol where
    show (Symbol name arity) = name ++ '/':show arity

data TermNode s = TermNode ID (Maybe Symbol) (Maybe (VarNode s)) (MVector s (TermNode s)) (STRef s Bool)

instance Graph (TermNode s) where
    type State (TermNode s) = s
    uniqueID (TermNode ident _ _ _ _) = ident
    toGraph prefix seenRef (TermNode ident sym var children doneRef) = doIfUnseen seenRef ident ([], []) $ do
        let label = case var of
                        Just (VarNode _ sym' _ _ _) -> sym'
                        Nothing -> show $ fromJust sym
        done <- readSTRef doneRef
        let check = if done
                        then " ✓"
                        else ""
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

add :: Queue (VarNode s) -> VarNode s -> TermNode s -> ST s (Queue (VarNode s))
add queue v@(VarNode _ _ _ termsRef _) t = do
    ts <- readSTRef termsRef
    writeSTRef termsRef $ ttlCons t ts
    return $ if termCount ts == 1
                 then queuePush queue v
                 else queue

merge :: Queue (VarNode s) -> VarNode s -> VarNode s -> ST s (Queue (VarNode s))
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
                     then queuePush queue bigV
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

commonFrontier :: Queue (VarNode s) -> Maybe (V.Vector (TermNode s), Int) -> V.Vector (TermNode s) -> ST s (Either (TermNode s, TermNode s) (Queue (VarNode s)))
commonFrontier queue parentsIndex t_list = do
    let t@(TermNode _ (Just (Symbol _ arity)) _ _ _) = t_list V.! 0
    case checkEqual t t_list of
        Just err -> return $ Left err
        Nothing  -> foldM goChild (Right queue) [0 .. arity - 1]
  where
    checkEqual t@(TermNode _ sym _ _ _) ts = let diffs = V.filter (\(TermNode _ sym' _ _ _) -> sym /= sym') ts
                                             in if V.null diffs
                                                    then Nothing
                                                    else Just (t, diffs V.! 0)
    goChild (Left err) _ = return $ Left err
    goChild (Right queue') i = do
        t0_list <- ithChildren i
        case firstVarNode t0_list of
            Nothing -> commonFrontier queue' (Just (t_list, i)) t0_list
            Just (j, (TermNode _ _ (Just varNode) _ _)) -> do
                case parentsIndex of
                    Nothing -> return ()
                    Just (parents, index) -> swapChild parents index j
                v <- rep varNode
                queue''  <- V.foldM (processVars j v) queue' $ V.indexed t0_list
                queue''' <- V.foldM (processTerms  v) queue'' t0_list
                return $ Right queue'''
            Just _ -> error "firstVarNode returned a TermNode with no varNode"
    ithChildren i = V.forM t_list $ \(TermNode _ _ _ children _) -> MV.read children i
    firstVarNode ts = (V.!? 0) . V.filter (\(_, TermNode _ _ varNode _ _) -> isJust varNode) $ V.indexed ts
    swapChild parents index j = do
        let (TermNode _ _ _ child0 _) = parents V.! 0
            (TermNode _ _ _ childj _) = parents V.! j
        tmp <- MV.read child0 index
        MV.write child0 index =<< MV.read childj index
        MV.write childj index tmp
    processVars j _ queue' (k, _) | k == j = return queue'
    processVars _ _ queue' (_, TermNode _ _ Nothing _ _) = return queue'
    processVars _ v queue' (_, TermNode _ _ (Just varNode) _ _) = do
        v2 <- rep varNode
        if uniqueID v /= uniqueID v2
            then merge queue' v v2
            else return queue'
    processTerms _ queue' (TermNode _ _ (Just _) _ _) = return queue'
    processTerms v queue' term = add queue' v term

unify :: V.Vector (TermNode s) -> ST s (Maybe (TermNode s, TermNode s))
unify t_list = do
    res <- commonFrontier queueEmpty Nothing t_list
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ queuePop queue
  where
    go Nothing = return Nothing
    go (Just (VarNode _ _ _ termsRef _, queue')) = do
        terms <- readSTRef termsRef
        if termCount terms < 2
            then go $ queuePop queue'
            else do
                t <- V.freeze =<< ttlToVector terms
                writeSTRef termsRef . singleton $ t V.! 0
                res <- commonFrontier queue' Nothing t
                case res of
                    Left  err     -> return $ Just err
                    Right queue'' -> go $ queuePop queue''

main :: IO ()
main = putStrLn $ runST $ do
    counter <- newSTRef (0 :: Int)
    let unique = do
            modifySTRef' counter (+1)
            readSTRef counter
        varNode v = VarNode <$> unique <*> pure v <*> newSTRef Nothing <*> newSTRef TTLEmpty <*> newSTRef 1
        varTerm v = TermNode <$> unique <*> pure Nothing <*> (Just <$> varNode v) <*> MV.new 0 <*> newSTRef False
        funcTerm sym children = TermNode <$> unique <*> pure (Just sym) <*> pure Nothing <*> V.thaw (V.fromList children) <*> newSTRef False
    let f = Symbol "f" 2
    x <- varTerm "x"
    z <- varTerm "z"
    expr1 <- funcTerm f [x, x]
    expr2 <- funcTerm f =<< sequence [funcTerm f [z, z], funcTerm f [z, x]]
    initial1 <- showSubgraph "initial1" expr1
    initial2 <- showSubgraph "initial2" expr2
    err <- unify $ V.fromList [expr1, expr2]
    case err of
        Just (t1, t2) -> do
            term1 <- showSubgraph "term1" t1
            term2 <- showSubgraph "term2" t2
            return $ "ERROR Could not match:\ndigraph {" ++ term1 ++ term2 ++ "}"
        Nothing -> do
            unify1 <- showSubgraph "unify1" expr1
            unify2 <- showSubgraph "unify2" expr2
            substitute expr1
            substitute expr2
            substitute1 <- showSubgraph "substitute1" expr1
            substitute2 <- showSubgraph "substitute2" expr2
            return $ intercalate "\n" ["digraph {", initial1, initial2, unify1, unify2, substitute1, substitute2, "}"]
