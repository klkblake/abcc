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
import Data.Monoid
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Numeric

import Queue (Queue)
import qualified Queue as Q
import TTList (TTList, Tagged)
import qualified TTList as TTL

type ID = Int

data DotNode = DotNode String ID String

instance Show DotNode where
    showsPrec _ (DotNode prefix ident label) = showString prefix . showChar '_' . showInt ident . showString " [label=\"" . showString label . showString "\"]"

data Edge = Edge String ID ID String

instance Show Edge where
    showsPrec _ (Edge prefix from to label) = showID from . showString " -> " . showID to . showString " [label=\"" . showString label . showString "\"]"
      where
        showID ident = showString prefix . showChar '_' . showInt ident

class Graph g where
    type State g
    uniqueID :: g -> ID
    toGraph :: String -> STRef (State g) IntSet -> g -> ST (State g) ([DotNode], [Edge])

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

data Symbol = Symbol String Int
            deriving Eq

instance Show Symbol where
    show (Symbol name arity) = name ++ '/':show arity

data Node s = Term ID Symbol (MVector s (Node s)) (STRef s Bool)
            | Var' (Var s)

instance Tagged (Node s) where
    type Tag (Node s) = Sum Int
    tag (Term _ _ _ _) = Sum 1
    tag (Var' _) = Sum 0

instance Graph (Node s) where
    type State (Node s) = s
    uniqueID (Term ident _ _ _) = ident
    uniqueID (Var' var) = uniqueID var
    toGraph prefix seenRef (Var' var) = toGraph prefix seenRef var
    toGraph prefix seenRef (Term ident sym children doneRef) = doIfUnseen seenRef ident ([], []) $ do
        done <- readSTRef doneRef
        let check = if done
                        then " ✓"
                        else ""
            node = DotNode prefix ident $ show sym ++ check
            edge = Edge prefix ident
        children' <- V.toList <$> V.freeze children
        let edges = zipWith (\c i -> edge (uniqueID c) $ '#':show i) children' [1 :: Int ..]
        (cns, ces) <- unzip <$> mapM (toGraph prefix seenRef) children'
        return (node:concat cns, edges ++ concat ces)

substitute :: Node s -> ST s ()
substitute (Var' _) = error "Called substitute on var"
substitute (Term _ _ children doneRef) = do
    done <- readSTRef doneRef
    if done
        then return ()
        else do
            writeSTRef doneRef True
            V.sequence_ . V.imap go =<< V.freeze children
  where
    go _ c@(Term _ _ _ _) = substitute c
    go i (Var' var) = do
        Var _ _ _ terms _ <- rep var
        ts <- TTL.toVector <$> readSTRef terms
        when (V.length ts /= 1) $ error "Attempted to substitute variable with multiple terms"
        MV.write children i $ ts V.! 0

data Var s = Var ID String (STRef s (Maybe (Var s))) (STRef s (TTList (Node s))) (STRef s Int)

instance Graph (Var s) where
    type State (Var s) = s
    uniqueID (Var ident _ _ _ _) = ident
    toGraph prefix seenRef (Var ident sym repRef terms varCountRef) = doIfUnseen seenRef ident ([], []) $ do
        varCount <- readSTRef varCountRef
        let node = DotNode prefix ident $ sym ++ " (" ++ show varCount ++ ")"
        rep' <- readSTRef repRef
        let edge = Edge prefix ident
        (rns, res) <- case rep' of
                          Just r -> (\(x, y) -> (x, edge (uniqueID r) "rep":y)) <$> toGraph prefix seenRef r
                          Nothing -> return ([], [])
        ts <- V.toList . TTL.toVector <$> readSTRef terms
        let edges = zipWith (\c i -> edge (uniqueID c) $ '#':show i) ts [1 :: Int ..]
        (cns, ces) <- unzip <$> mapM (toGraph prefix seenRef) ts
        return (node:rns ++ concat cns, edges ++ res ++ concat ces)

add :: Queue (Var s) -> Var s -> Node s -> ST s (Queue (Var s))
add queue v@(Var _ _ _ termsRef _) t = do
    ts <- readSTRef termsRef
    writeSTRef termsRef $ TTL.cons t ts
    return $ if TTL.tag ts == 1
                 then Q.push queue v
                 else queue

merge :: Queue (Var s) -> Var s -> Var s -> ST s (Queue (Var s))
merge queue v1@(Var _ _ _ _ varCountRef1) v2@(Var _ _ repRef _ varCountRef2) = do
    r1 <- readSTRef varCountRef1
    r2 <- readSTRef varCountRef2
    let vc = r1 + r2
    if r1 >= r2
        then go vc v1 v2
        else go vc v2 v1
  where
    go vc bigV@(Var _ _ _ bigTermsRef bigVarCountRef) (Var _ _ _ termsRef varCountRef) = do
        bigTerms <- readSTRef bigTermsRef
        terms    <- readSTRef termsRef
        let bigTerms' = bigTerms `TTL.concat` terms
        writeSTRef bigTermsRef bigTerms'
        writeSTRef repRef (Just bigV)
        writeSTRef termsRef TTL.empty
        writeSTRef varCountRef 0
        writeSTRef bigVarCountRef vc
        return $ if TTL.tag bigTerms <= 1 && TTL.tag bigTerms' >= 2
                     then Q.push queue bigV
                     else queue

rep :: Var s -> ST s (Var s)
rep v = do
    r <- findRep v
    setRep r v
  where
    findRep v'@(Var _ _ repRef _ _) = do
        r <- readSTRef repRef
        case r of
            Just r' -> findRep r'
            Nothing -> return v'
    setRep r (Var _ _ repRef _ _) = do
        r' <- readSTRef repRef
        case r' of
            Just r'' -> writeSTRef repRef (Just r) >> setRep r r''
            Nothing  -> return r

commonFrontier :: Queue (Var s) -> Maybe (V.Vector (Node s), Int) -> V.Vector (Node s) -> ST s (Either (Node s, Node s) (Queue (Var s)))
commonFrontier queue parentsIndex t_list = do
    let t@(Term _ (Symbol _ arity) _ _) = t_list V.! 0
    case checkEqual t t_list of
        Just err -> return $ Left err
        Nothing  -> foldM goChild (Right queue) [0 .. arity - 1]
  where
    checkEqual (Var' _) _ = error "Var passed to commonFrontier"
    checkEqual t@(Term _ sym _ _) ts =
        let diffs = V.filter (\(Term _ sym' _ _) -> sym /= sym') ts
        in if V.null diffs
               then Nothing
               else Just (t, diffs V.! 0)
    goChild (Left err) _ = return $ Left err
    goChild (Right queue') i = do
        t0_list <- ithChildren i
        case firstVar t0_list of
            Nothing -> commonFrontier queue' (Just (t_list, i)) t0_list
            Just (j, var) -> do
                case parentsIndex of
                    Nothing -> return ()
                    Just (parents, index) -> swapChild parents index j
                v <- rep var
                queue''  <- V.foldM (processVars j v) queue' $ V.indexed t0_list
                queue''' <- V.foldM (processTerms  v) queue'' t0_list
                return $ Right queue'''
    ithChildren i = V.forM t_list $ \(Term _ _ children _) -> MV.read children i
    firstVar ts = listToMaybe . catMaybes . map toVar . zip [0 :: Int ..] $ V.toList ts
    toVar (_, (Term _ _ _ _)) = Nothing
    toVar (i, (Var' var)) = Just (i, var)
    swapChild parents index j = do
        let (Term _ _ child0 _) = parents V.! 0
            (Term _ _ childj _) = parents V.! j
        tmp <- MV.read child0 index
        MV.write child0 index =<< MV.read childj index
        MV.write childj index tmp
    processVars j _ queue' (k, _) | k == j = return queue'
    processVars _ _ queue' (_, Term _ _ _ _) = return queue'
    processVars _ v queue' (_, Var' var) = do
        v2 <- rep var
        if uniqueID v /= uniqueID v2
            then merge queue' v v2
            else return queue'
    processTerms _ queue' (Var' _) = return queue'
    processTerms v queue' term = add queue' v term

unify :: V.Vector (Node s) -> ST s (Maybe (Node s, Node s))
unify t_list = do
    res <- commonFrontier Q.empty Nothing t_list
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ Q.pop queue
  where
    go Nothing = return Nothing
    go (Just (Var _ _ _ termsRef _, queue')) = do
        terms <- readSTRef termsRef
        if TTL.tag terms < 2
            then go $ Q.pop queue'
            else do
                let t = TTL.toVector terms
                writeSTRef termsRef . TTL.singleton $ t V.! 0
                res <- commonFrontier queue' Nothing t
                case res of
                    Left  err     -> return $ Just err
                    Right queue'' -> go $ Q.pop queue''

main :: IO ()
main = putStrLn $ runST $ do
    counter <- newSTRef (0 :: Int)
    let unique = do
            modifySTRef' counter (+1)
            readSTRef counter
        varNode v = Var <$> unique <*> pure v <*> newSTRef Nothing <*> newSTRef TTL.empty <*> newSTRef 1
        varTerm v = Var' <$> varNode v
        funcTerm sym children = Term <$> unique <*> pure sym <*> V.thaw (V.fromList children) <*> newSTRef False
    let f = Symbol "f" 2
        unifyTerm x y = funcTerm (Symbol "unify" 2) [x, y]
        errorTerm x y = funcTerm (Symbol "Could not unify" 2) [x, y]
    x <- varTerm "x"
    z <- varTerm "z"
    expr1 <- funcTerm f [x, x]
    expr2 <- funcTerm f =<< sequence [funcTerm f [z, z], funcTerm f [z, x]]
    g1 <- showSubgraph "initial" =<< unifyTerm expr1 expr2
    err <- unify $ V.fromList [expr1, expr2]
    case err of
        Just (t1, t2) -> do
            e <- showSubgraph "error" =<< errorTerm t1 t2
            return $ "digraph {" ++ e ++ "}"
        Nothing -> do
            g2 <- showSubgraph "unify" =<< unifyTerm expr1 expr2
            substitute expr1
            substitute expr2
            g3 <- showSubgraph "substitute" =<< unifyTerm expr1 expr2
            return $ intercalate "\n" ["digraph {", g1, g2, g3, "}"]
