{-# LANGUAGE TypeFamilies, TupleSections #-}
module Main where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Functor.Identity
import Data.List
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

import Op

import GraphViz
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

data Stage = New | DeloopSeen | Delooped | Substituted deriving Eq

data VarType = Structural | Substructural deriving Eq

data Term s = Term ID Symbol (MVector s (RNode s)) (STRef s Stage)

data Var s = Var ID String VarType (STRef s (Maybe (Var s))) (STRef s (TreeList (RNode s))) (STRef s Int)

newtype RNode s = RNode (STRef s (Either (Term s) (Var s)))

instance GraphViz (Term s) where
    type State (Term s) = s
    toNode (Term ident sym children stageRef) = Node ident label labelledChildren
      where
        label = do
            stage <- readSTRef stageRef
            return $ show sym
        labelledChildren = zip (map (('#':) . show) [1 :: Int ..]) . map toNode . V.toList <$> (V.mapM fromRNode =<< V.freeze children)

instance GraphViz (Var s) where
    type State (Var s) = s
    toNode (Var ident sym ty repRef terms varCountRef) = Node ident label labelledChildren
      where
        label = do
            varCount <- readSTRef varCountRef
            return $ sym ++ " (" ++ show varCount ++ ")" ++ if ty == Structural then "" else " kf"
        labelledChildren = do
            rep' <- readSTRef repRef
            let repEdge = case rep' of
                              Just r -> [("rep", toNode r)]
                              Nothing -> []
            (repEdge ++) . zip (map (('#':) . show) [1 :: Int ..]) . map toNode <$> (mapM fromRNode =<< V.toList . TL.toVector <$> readSTRef terms)

mkTerm :: ST s ID -> Symbol -> [RNode s] -> ST s (Term s)
mkTerm unique sym children = Term <$> unique <*> pure sym <*> V.thaw (V.fromList children) <*> newSTRef New

mkVar :: ST s ID -> String -> VarType -> ST s (Var s)
mkVar unique v ty = Var <$> unique <*> pure v <*> pure ty <*> newSTRef Nothing <*> newSTRef TL.empty <*> newSTRef 1

toRNode :: Either (Term s) (Var s) -> ST s (RNode s)
toRNode node = RNode <$> newSTRef node

mkRTerm :: ST s ID -> Symbol -> [RNode s] -> ST s (RNode s)
mkRTerm unique sym children = toRNode =<< Left <$> mkTerm unique sym children

mkRVar :: ST s ID -> String -> VarType -> ST s (RNode s)
mkRVar unique v ty = toRNode =<< Right <$> mkVar unique v ty

fromRNode :: RNode s -> ST s (Either (Term s) (Var s))
fromRNode (RNode ref) = readSTRef ref

fromRTerm :: RNode s -> ST s (Term s)
fromRTerm t = do
    Left t' <- fromRNode t
    return t'

fromRVar :: RNode s -> ST s (Var s)
fromRVar v = do
    Right v' <- fromRNode v
    return v'

matchingAttrib :: Bool -> Term s -> Bool
matchingAttrib a (Term _ (Attrib a') _ _) = a == a'
matchingAttrib _ _ = False

replaceNode :: RNode s -> Either (Term s) (Var s) -> ST s ()
replaceNode (RNode ref) node = writeSTRef ref node

add :: Queue (Var s) -> Var s -> RNode s -> ST s (Queue (Var s))
add queue v@(Var _ _ ty _ termsRef _) t = do
    ts <- readSTRef termsRef
    writeSTRef termsRef $ TL.cons t ts
    return $ if TL.size ts == 1 && ty == Structural
                 then Q.push queue v
                 else queue

merge :: Queue (Var s) -> Var s -> Var s -> ST s (Queue (Var s))
merge queue v1@(Var _ _ _ _ _ varCountRef1) v2@(Var _ _ _ _ _ varCountRef2) = do
    r1 <- readSTRef varCountRef1
    r2 <- readSTRef varCountRef2
    let vc = r1 + r2
    if r1 >= r2
        then go vc v1 v2
        else go vc v2 v1
  where
    go vc bigV@(Var _ _ ty _ bigTermsRef bigVarCountRef) (Var _ _ _ repRef termsRef varCountRef) = do
        bigTerms <- readSTRef bigTermsRef
        terms    <- readSTRef termsRef
        let bigTerms' = bigTerms `TL.concat` terms
        writeSTRef bigTermsRef bigTerms'
        writeSTRef repRef (Just bigV)
        writeSTRef termsRef TL.empty
        writeSTRef varCountRef 0
        writeSTRef bigVarCountRef vc
        return $ if TL.size bigTerms <= 1 && TL.size bigTerms' >= 2 && ty == Structural
                     then Q.push queue bigV
                     else queue

rep :: Var s -> ST s (Var s)
rep v = do
    r <- findRep v
    setRep r v
  where
    findRep v'@(Var _ _ _ repRef _ _) = do
        r <- readSTRef repRef
        case r of
            Just r' -> findRep r'
            Nothing -> return v'
    setRep r (Var _ _ _ repRef _ _) = do
        r' <- readSTRef repRef
        case r' of
            Just r'' -> writeSTRef repRef (Just r) >> setRep r r''
            Nothing  -> return r

splitNodes :: V.Vector (RNode s) -> ST s ([RNode s], [(Int, RNode s)])
splitNodes = V.foldM splitNode ([], []) . V.indexed
  where
    splitNode (ls, rs) (i, n) = do
        n' <- fromRNode n
        return $ case n' of
                     Left  _ -> (n:ls, rs)
                     Right _ -> (ls, (i, n):rs)

commonFrontier :: ST s ID -> Queue (Var s) -> V.Vector (Term s) -> ST s (Either (Term s, Term s) (Queue (Var s)))
commonFrontier unique queue t_list = do
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
        sns <- splitNodes t0_list
        case sns of
            ([], []) -> error "commonFrontier called on empty term list"
            (t:terms, []) -> do
                Term _ sym _ _ <- fromRTerm t
                case sym of
                    Or       -> fmap Right . genSubVar queue' i $ t:terms
                    Attrib a -> do
                        terms' <- mapM fromRTerm terms
                        Right <$> if null $ filter (not . matchingAttrib a) terms'
                                      then return queue'
                                      else genSubVar queue' i $ t:terms
                    _ -> commonFrontier unique queue' . V.fromList =<< mapM fromRTerm (t:terms)
            (terms, (j, var):vars) -> do
                swapChild i j
                v <- rep =<< fromRVar var
                queue''  <- foldM (processVars  v) queue' $ map snd vars
                queue''' <- foldM (processTerms v) queue'' terms
                return $ Right queue'''
    ithChildren i = V.forM t_list $ \(Term _ _ children _) -> MV.read children i
    genSubVar queue' i terms = do
        v <- mkRVar unique "attr" Substructural
        v' <- fromRVar v
        queue'' <- foldM (\q -> add q v') queue' terms
        let Term _ _ cs _ = t_list V.! 0
        MV.write cs i v
        return queue''
    swapChild i j = do
        let Term _ _ c0 _ = t_list V.! 0
            Term _ _ cj _ = t_list V.! j
        t0 <- MV.read c0 i
        tj <- MV.read cj i
        MV.write c0 i tj
        MV.write cj i t0
    processVars v@(Var ident _ _ _ _ _) queue' var = do
        v2@(Var ident2 _ _ _ _ _) <- rep =<< fromRVar var
        if ident /= ident2
            then merge queue' v v2
            else return queue'
    processTerms v queue' = add queue' v

unify :: ST s ID -> V.Vector (Term s) -> ST s (Maybe (Term s, Term s))
unify unique t_list = do
    res <- commonFrontier unique Q.empty t_list
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ Q.pop queue
  where
    go Nothing = return Nothing
    go (Just (Var _ _ _ _ termsRef _, queue')) = do
        terms <- readSTRef termsRef
        if TL.size terms < 2
            then go $ Q.pop queue'
            else do
                let t = TL.toVector terms
                writeSTRef termsRef . TL.singleton $ t V.! 0
                res <- commonFrontier unique queue' =<< V.mapM fromRTerm t
                case res of
                    Left  err     -> return $ Just err
                    Right queue'' -> go $ Q.pop queue''

simplifyOr :: (RNode s -> RNode s -> ST s (RNode s)) -> RNode s -> RNode s -> ST s (RNode s)
simplifyOr f a b = do
    a' <- fromRNode a
    b' <- fromRNode b
    go a' b'
  where
    go (Left (Term _ (Attrib True)  _ _)) _ = return a
    go (Left (Term _ (Attrib False) _ _)) _ = return b
    go _ (Left (Term _ (Attrib True)  _ _)) = return b
    go _ (Left (Term _ (Attrib False) _ _)) = return a
    go (Left  (Term ident _ _ _))     (Left  (Term ident' _ _ _))     | ident == ident' = return a
    go (Right (Var  ident _ _ _ _ _)) (Right (Var  ident' _ _ _ _ _)) | ident == ident' = return a
    go _ _ = f a b

deloop :: ST s ID -> RNode s -> ST s ()
deloop unique node = do
    n <- fromRNode node
    case n of
        Left (Term _ sym children stageRef) -> do
            stage <- readSTRef stageRef
            case stage of
                New -> do
                    writeSTRef stageRef DeloopSeen
                    children' <- V.freeze children
                    V.mapM_ (deloop unique) children'
                    case sym of
                        Or -> do
                            a <- derefVar $ children' V.! 0
                            b <- derefVar $ children' V.! 1
                            replaceNode node =<< fromRNode =<< simplifyOr (const . const $ return node) a b
                        _  -> return ()
                    writeSTRef stageRef Delooped
                DeloopSeen -> case sym of
                                  Or -> replaceNode node =<< Left <$> mkTerm unique (Attrib False) []
                                  _  -> return ()
                _ -> return ()
        Right var -> do
            v@(Var _ _ _ _ terms _) <- rep var
            ts <- TL.toVector <$> readSTRef terms
            V.mapM_ (deloop unique) ts
            ts' <- TL.toVector <$> readSTRef terms
            (terms', vars) <- V.foldM splitVars ([], []) ts'
            terms'' <- mapM fromRTerm terms'
            writeSTRef terms =<< case terms'' of
                                     [] -> TL.singleton <$> mkRTerm unique (Attrib False) []
                                     Term _ (Attrib a) _ _:_ -> return $
                                         if all (matchingAttrib a) terms''
                                             then TL.singleton $ head terms'
                                             else TL.fromList terms'
                                     _ -> return $ TL.fromList terms'
            mapM_ (mergeVar v) vars
  where
    derefVar n = do
        n' <- fromRNode n
        case n' of
            Left _ -> return n
            Right v -> do
                Var _ _ _ _ terms _ <- rep v
                terms' <- readSTRef terms
                return $ if TL.size terms' == 1
                             then TL.toVector terms' V.! 0
                             else n
    splitVars (terms, vars) node' = do
        n <- fromRNode node'
        return $ case n of
                     Left  _ -> (node':terms, vars)
                     Right v -> (terms, v:vars)
    mergeVar v v' = do
            vr@(Var ident _ _ _ _ _) <- rep v
            vr'@(Var ident' _ _ _ _ _) <- rep v'
            when (ident /= ident') $ merge Q.empty vr vr' >> return ()

substitute :: Term s -> ST s ()
substitute (Term _ _ children stageRef) = do
    stage <- readSTRef stageRef
    unless (stage == Substituted) $ do
        writeSTRef stageRef Substituted
        V.sequence_ . V.imap go =<< V.mapM fromRNode =<< V.freeze children
  where
    go _ (Left term') = substitute term'
    go i (Right var) = do
        Var _ _ _ _ terms _ <- rep var
        ts <- TL.toVector <$> readSTRef terms
        when (V.length ts == 1) $ MV.write children i $ ts V.! 0
        V.mapM_ substitute =<< V.mapM fromRTerm ts

mkAttribTerm :: ST s ID -> Symbol -> Maybe Bool -> Maybe Bool -> [RNode s] -> ST s (RNode s)
mkAttribTerm unique sym relevant affine children = do
    t <- mkRTerm' sym children
    children' <- mapM fromRTerm children
    false <- mkRTerm' (Attrib False) []
    k  <- mkAttrib children' false relevant getRelevant
    f  <- mkAttrib children' false affine getAffine
    mkRTerm' Attribs [t, k, f]
  where
    mkRTerm' = mkRTerm unique
    getRelevant (Term _ _ cs _) = MV.read cs 1
    getAffine   (Term _ _ cs _) = MV.read cs 2
    mkAttrib _         _     (Just a) _   = mkRTerm' (Attrib a) []
    mkAttrib children' false Nothing  get = foldM (simplifyOr mkOr) false =<< mapM get children'
    mkOr a b = mkRTerm' Or [a, b]

mkAttribVar :: ST s ID -> String -> Maybe Bool -> Maybe Bool -> ST s (RNode s)
mkAttribVar unique name relevant affine = do
    v <- mkRVar unique name Structural
    k <- mkAttrib relevant "_k"
    f <- mkAttrib affine   "_f"
    mkRTerm unique Attribs [v, k, f]
  where
    mkAttrib (Just a) _      = mkRTerm unique (Attrib a) []
    mkAttrib Nothing  suffix = mkRVar unique (name ++ suffix) Substructural

opType :: ST s ID -> UntypedOp -> ST s (RNode s, RNode s)
opType unique opcode = op opcode
  where
    mkAttribTerm' sym children = mkAttribTerm unique sym Nothing Nothing children
    a' ~> b' = do
        (a'', vars) <- a' M.empty
        (b'', _)    <- b' vars
        return (a'', b'')
    infixr 1 ~>
    resolveVars action = \a' b' vars -> do
        (a'', vars')  <- a' vars
        (b'', vars'') <- b' vars'
        r <- action a'' b''
        return (r, vars'')
    (.*) = resolveVars $ \a' b' -> mkAttribTerm' Product [a', b']
    infixr 7 .*
    mkBlock = resolveVars $ \a' b' -> mkAttribTerm unique Block (Just False) (Just False) [a', b']
    mkBlock' a' b' vars = do
        let a'' = \vars -> return (a', vars)
        let b'' = \vars -> return (b', vars)
        (r, _) <- mkBlock a'' b'' M.empty
        return (r, vars)
    unit = \vars -> (, vars) <$> mkAttribTerm' Unit []

    var  v = var' v Nothing Nothing
    var' v relevant affine = \vars ->
        case M.lookup v vars of
            Just v' -> return (v', vars)
            Nothing -> do
                v' <- mkAttribVar unique v relevant affine
                return (v', M.insert v v' vars)
    a = var "a"
    b = var "b"
    c = var "c"
    d = var "d"
    e = var "e"
    s = var "s"
    xDrop = var' "x" (Just False) Nothing
    xCopy = var' "x" Nothing (Just False)

    op (LitBlock block) = do
        tys <- mapM (opType unique . runIdentity) block
        case tys of
            [] -> s ~> mkBlock a a .* s
            _  -> do
                let (a', _)  = head tys
                    (_,  b') = last tys
                s ~> mkBlock' a' b' .* s

    op AssocL = a .* b .* c ~> (a .* b) .* c
    op AssocR = (a .* b) .* c ~> a .* b .* c
    op Swap   = a .* b .* c ~> b .* a .* c
    op SwapD  = a .* b .* c .* d ~> a .* c .* b .* d
    op Intro1 = a ~> a .* unit
    op Elim1  = a .* unit ~> a
    op Drop   = xDrop .* e ~> e
    op Copy   = xCopy .* e ~> xCopy .* xCopy .* e

main :: IO ()
main = putStrLn $ runST $ do
    counter <- newSTRef (0 :: Int)
    let unique = do
            modifySTRef' counter (+1)
            readSTRef counter
        varNode v = mkAttribVar unique v Nothing Nothing
        term sym children = mkAttribTerm unique sym Nothing Nothing children
    let showTerm label xs = showSubgraph label =<< mkTerm unique (Sealed label) xs
        errorTerm x y = mkTerm unique (Sealed "Could not unify") =<< mapM (toRNode . Left) [x, y]
    exprs <- mapM (opType unique) [Intro1, AssocR, Swap, AssocL, Elim1, LitBlock [], LitBlock $ map Identity [AssocL, AssocR]]
    let flatExprs = concatMap (\(a, b) -> [a, b]) exprs
    g1 <- showTerm "initial" flatExprs
    let unifyPair (Just err) _      = return $ Just err
        unifyPair Nothing    (a, b) = unify unique =<< V.mapM fromRTerm (V.fromList [a, b])
    err <- foldM unifyPair Nothing . zip (map snd exprs) . map fst $ tail exprs
    case err of
        Just (t1, t2) -> do
            e <- showSubgraph "error" =<< errorTerm t1 t2
            return $ "digraph {" ++ e ++ "}"
        Nothing -> do
            g2 <- showTerm "unify" flatExprs
            mapM_ (deloop unique) flatExprs
            g3 <- showTerm "deloop" flatExprs
            mapM_ substitute =<< mapM fromRTerm flatExprs
            g4 <- showTerm "substitute" flatExprs
            return $ intercalate "\n" ["digraph {", g1, g2, g3, g4, "}"]
