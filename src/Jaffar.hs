{-# LANGUAGE TypeFamilies #-}
module Main where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Either
import Data.List
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

import Dot
import TreeList (TreeList)
import qualified TreeList as TL
import Queue (Queue)
import qualified Queue as Q

data Symbol = Product
            | Sum
            | Block
            | Num
            | Unit
            | Void
            | Sealed String
            | Attribs
            | Or
            | Attrib Bool
            deriving Eq

instance Show Symbol where
    show Product       = "*"
    show Sum           = "+"
    show Block         = "[ -> ]"
    show Num           = "N"
    show Unit          = "1"
    show Void          = "0"
    show (Sealed seal) = '{' : seal ++ "}"
    show Attribs       = "tkf"
    show Or            = "∨"
    show (Attrib attr) = show attr

arity :: Symbol -> Int
arity Product    = 2
arity Sum        = 2
arity Block      = 2
arity Num        = 0
arity Unit       = 0
arity Void       = 0
arity (Sealed _) = 1
arity Attribs    = 3
arity Or         = 2
arity (Attrib _) = 0

data Term s = Term ID Symbol (MVector s (Either (Term s) (Var s))) (STRef s Bool)

instance Graph (Term s) where
    type State (Term s) = s
    toNode (Term ident sym children doneRef) = Node ident label labelledChildren
      where
        label = do
            done <- readSTRef doneRef
            return $ show sym ++ if done then " ✓" else ""
        labelledChildren = zip (map (('#':) . show) [1 :: Int ..]) . map toNode . V.toList <$> V.freeze children

mkTerm :: ID -> Symbol -> [Either (Term s) (Var s)] -> ST s (Term s)
mkTerm ident sym children = Term ident sym <$> V.thaw (V.fromList children) <*> newSTRef False

substitute :: Term s -> ST s ()
substitute (Term _ _ children doneRef) = do
    done <- readSTRef doneRef
    unless done $ do
        writeSTRef doneRef True
        V.sequence_ . V.imap go =<< V.freeze children
  where
    go _ (Left term) = substitute term
    go i (Right var) = do
        Var _ _ _ terms _ <- rep var
        ts <- TL.toVector <$> readSTRef terms
        when (V.length ts /= 1) $ error "Attempted to substitute variable with multiple terms"
        MV.write children i . Left $ ts V.! 0

data Var s = Var ID String (STRef s (Maybe (Var s))) (STRef s (TreeList (Term s))) (STRef s Int)

instance Graph (Var s) where
    type State (Var s) = s
    toNode (Var ident sym repRef terms varCountRef) = Node ident label labelledChildren
      where
        label = do
            varCount <- readSTRef varCountRef
            return $ sym ++ " (" ++ show varCount ++ ")"
        labelledChildren = do
            rep' <- readSTRef repRef
            let repEdge = case rep' of
                              Just r -> [("rep", toNode r)]
                              Nothing -> []
            (repEdge ++) . zip (map (('#':) . show) [1 :: Int ..]) . map toNode . V.toList . TL.toVector <$> readSTRef terms

mkVar :: ID -> String -> ST s (Var s)
mkVar ident v = Var ident v <$> newSTRef Nothing <*> newSTRef TL.empty <*> newSTRef 1

add :: Queue (Var s) -> Var s -> Term s -> ST s (Queue (Var s))
add queue v@(Var _ _ _ termsRef _) t = do
    ts <- readSTRef termsRef
    writeSTRef termsRef $ TL.cons t ts
    return $ if TL.size ts == 1
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
        let bigTerms' = bigTerms `TL.concat` terms
        writeSTRef bigTermsRef bigTerms'
        writeSTRef repRef (Just bigV)
        writeSTRef termsRef TL.empty
        writeSTRef varCountRef 0
        writeSTRef bigVarCountRef vc
        return $ if TL.size bigTerms <= 1 && TL.size bigTerms' >= 2
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

commonFrontier :: Queue (Var s) -> Maybe (V.Vector (Term s), Int) -> V.Vector (Term s) -> ST s (Either (Term s, Term s) (Queue (Var s)))
commonFrontier queue parentsIndex t_list = do
    let t@(Term _ sym _ _) = t_list V.! 0
    case checkEqual t t_list of
        Just err -> return $ Left err
        Nothing  -> foldM goChild (Right queue) [0 .. arity sym - 1]
  where
    checkEqual t@(Term _ sym _ _) ts =
        let diffs = V.filter (\(Term _ sym' _ _) -> sym /= sym') ts
        in if V.null diffs
               then Nothing
               else Just (t, diffs V.! 0)
    goChild (Left err) _ = return $ Left err
    goChild (Right queue') i = do
        t0_list <- ithChildren i
        case splitNodes t0_list of
            (terms, []) -> commonFrontier queue' (Just (t_list, i)) . V.fromList $ terms
            (terms, (j, var):vars) -> do
                case parentsIndex of
                    Nothing -> return ()
                    Just (parents, index) -> swapChild parents index j
                v <- rep var
                queue''  <- foldM (processVars  v) queue' $ map snd vars
                queue''' <- foldM (processTerms v) queue'' terms
                return $ Right queue'''
    ithChildren i = V.forM t_list $ \(Term _ _ children _) -> MV.read children i
    splitNodes ts = partitionEithers . zipWith toEither [0 :: Int ..] $ V.toList ts
    toEither _ (Left  term) = Left  term
    toEither i (Right var)  = Right (i, var)
    swapChild parents index j = do
        let (Term _ _ child0 _) = parents V.! 0
            (Term _ _ childj _) = parents V.! j
        tmp <- MV.read child0 index
        MV.write child0 index =<< MV.read childj index
        MV.write childj index tmp
    processVars v@(Var ident _ _ _ _) queue' var = do
        v2@(Var ident2 _ _ _ _) <- rep var
        if ident /= ident2
            then merge queue' v v2
            else return queue'
    processTerms v queue' = add queue' v

unify :: V.Vector (Term s) -> ST s (Maybe (Term s, Term s))
unify t_list = do
    res <- commonFrontier Q.empty Nothing t_list
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ Q.pop queue
  where
    go Nothing = return Nothing
    go (Just (Var _ _ _ termsRef _, queue')) = do
        terms <- readSTRef termsRef
        if TL.size terms < 2
            then go $ Q.pop queue'
            else do
                let t = TL.toVector terms
                writeSTRef termsRef . TL.singleton $ t V.! 0
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
        var v = unique >>= \ident -> mkVar ident v
        varNode v = Right <$> var v
        term sym children = unique >>= \ident -> mkTerm ident sym children
        termNode sym children = Left <$> term sym children
    let unifyTerm x y = term (Sealed "unify") [Left x, Left y]
        errorTerm x y = term (Sealed "Could not unify") [Left x, Left y]
    x <- varNode "x"
    z <- varNode "z"
    expr1 <- term Product [x, x]
    expr2 <- term Product =<< sequence [termNode Product [z, z], termNode Product [z, x]]
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
