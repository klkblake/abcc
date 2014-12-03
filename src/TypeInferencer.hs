{-# LANGUAGE TypeFamilies, TupleSections, FlexibleInstances, FlexibleContexts, RecursiveDo #-}
module TypeInferencer
    ( inferTypes
    , TIStage (..)
    ) where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import qualified Data.HashTable.ST.Basic as H
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Vector as V
import Pipes hiding ((~>))

import Op
import qualified Type as T

import GraphViz
import qualified InterList as IL
import ShortList (ShortList)
import qualified ShortList as SL
import TreeList (TreeList)
import qualified TreeList as TL
import Queue (Queue)
import qualified Queue as Q

data TIStage = TIInitial
             | TIUnified
             | TIResolved
             | TISubstituted
             deriving (Eq, Enum, Bounded, Show)

data Symbol = Product
            | Sum
            | Block
            | Num
            | Unit
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
arity (Sealed _) = 1
arity Attribs    = 3
arity Or         = 2
arity (Attrib _) = 0

data Stage = New | DeloopSeen | Delooped | Substituted deriving Eq

data VarType = Structural | Substructural | Void deriving Eq

data Term s = Term {-# UNPACK #-} !ID Symbol {-# UNPACK #-} !(ShortList (RNode s)) {-# UNPACK #-} !(STRef s Stage)

data Var s = Var {-# UNPACK #-} !ID String {-# UNPACK #-} !(STRef s VarType) {-# UNPACK #-} !(STRef s (Maybe (Var s))) {-# UNPACK #-} !(STRef s (TreeList (RNode s))) {-# UNPACK #-} !(STRef s (TreeList (RNode s, RNode s))) {-# UNPACK #-} !(STRef s Int)

newtype RNode s = RNode (STRef s (Either (Term s) (Var s)))

instance GraphViz (Term s) where
    type State (Term s) = s
    toNode mode (Term ident sym children _) =
        case mode of
            Verbose -> toNode'
            Compact -> case sym of
                           Attribs -> do
                               [t, k, f] <- mapM fromRNode $ SL.toList children
                               Node _ label' labelledChildren' <- toNode mode t
                               let (kl, kc) = case k of
                                                  Left (Term _ (Attrib True)  _ _) -> ("k", [])
                                                  Left (Term _ (Attrib False) _ _) -> ("", [])
                                                  _ -> ("", [("k", toNode mode k)])
                               let (fl, fc) = case f of
                                                  Left (Term _ (Attrib True)  _ _) -> ("f", [])
                                                  Left (Term _ (Attrib False) _ _) -> ("", [])
                                                  _ -> ("", [("f", toNode mode f)])
                               return $ Node ident (label' ++ " " ++ kl ++ fl) $ kc ++ fc ++ labelledChildren'
                           _ -> toNode'
      where
        toNode' = Node ident label <$> labelledChildren
        label = show sym
        labelledChildren = zip (map (('#':) . show) [1 :: Int ..]) . map (toNode mode) <$> mapM fromRNode (SL.toList children)

instance GraphViz (Var s) where
    type State (Var s) = s
    toNode mode (Var ident sym tyRef repRef terms mergesRef varCountRef) =
        case mode of
            Verbose -> toNode'
            Compact -> do
                rep' <- readSTRef repRef
                case rep' of
                    Just r -> toNode mode r
                    Nothing -> toNode'
      where
        toNode' = Node ident <$> label <*> labelledChildren
        label = do
            ty <- readSTRef tyRef
            varCount <- readSTRef varCountRef
            return $ sym ++ " (" ++ show varCount ++ ")" ++ case ty of
                                                                Structural    -> ""
                                                                Substructural -> " kf"
                                                                Void          -> " void"
        labelledChildren = do
            rep' <- readSTRef repRef
            let repEdge = case rep' of
                              Just r -> [("rep", toNode mode r)]
                              Nothing -> []
            termEdges <- zip numbers <$> (mapM fromRNode' =<< V.toList . TL.toVector <$> readSTRef terms)
            (mergesLeft, mergesRight) <- unzip . V.toList . TL.toVector <$> readSTRef mergesRef
            mergesLeftEdges  <- zip (map ("ML" ++) numbers) <$> mapM fromRNode' mergesLeft
            mergesRightEdges <- zip (map ("MR" ++) numbers) <$> mapM fromRNode' mergesRight
            return $ repEdge ++ termEdges ++ mergesLeftEdges ++ mergesRightEdges
        numbers = map (('#':) . show) [1 :: Int ..]
        fromRNode' rn = toNode mode <$> fromRNode rn

mkTerm :: ST s ID -> Symbol -> [RNode s] -> ST s (Term s)
mkTerm unique sym children = Term <$> unique <*> pure sym <*> pure (SL.fromList children) <*> newSTRef New

mkVar :: ST s ID -> String -> VarType -> ST s (Var s)
mkVar unique v ty = Var <$> unique <*> pure v <*> newSTRef ty <*> newSTRef Nothing <*> newSTRef TL.empty <*> newSTRef TL.empty <*> newSTRef 1

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

getType :: RNode s -> ST s (RNode s)
getType node = do
    (Term _ _ cs _) <- fromRTerm node
    return $ cs SL.! 0

getAttribs :: RNode s -> ST s (RNode s, RNode s)
getAttribs node = do
    (Term _ _ cs _) <- fromRTerm node
    return (cs SL.! 1, cs SL.! 2)

replaceNode :: RNode s -> Either (Term s) (Var s) -> ST s ()
replaceNode (RNode ref) = writeSTRef ref

add :: Queue (Var s) -> Var s -> RNode s -> ST s (Queue (Var s))
add queue v@(Var _ _ tyRef _ termsRef _ _) t = do
    ty <- readSTRef tyRef
    ts <- readSTRef termsRef
    writeSTRef termsRef $ TL.cons t ts
    return $ if TL.size ts == 1 && ty /= Substructural
                 then Q.push queue v
                 else queue

merge :: Queue (Var s) -> Var s -> Var s -> ST s (Queue (Var s))
merge queue v1@(Var _ _ _ _ _ _ varCountRef1) v2@(Var _ _ _ _ _ _ varCountRef2) = do
    r1 <- readSTRef varCountRef1
    r2 <- readSTRef varCountRef2
    let vc = r1 + r2
    if r1 >= r2
        then go vc v1 v2
        else go vc v2 v1
  where
    go vc bigV@(Var _ _ bigTyRef _ bigTermsRef bigMergesRef bigVarCountRef) (Var _ _ tyRef repRef termsRef mergesRef varCountRef) = do
        bigTy     <- readSTRef bigTyRef
        ty        <- readSTRef tyRef
        bigTerms  <- readSTRef bigTermsRef
        terms     <- readSTRef termsRef
        bigMerges <- readSTRef bigMergesRef
        merges    <- readSTRef mergesRef
        let bigTerms' = bigTerms `TL.concat` terms
        writeSTRef bigTyRef $ if ty == Void then ty else bigTy
        writeSTRef bigTermsRef bigTerms'
        writeSTRef bigMergesRef $ bigMerges `TL.concat` merges
        writeSTRef repRef (Just bigV)
        writeSTRef termsRef TL.empty
        writeSTRef varCountRef 0
        writeSTRef bigVarCountRef vc
        return $ if TL.size bigTerms <= 1 && TL.size bigTerms' >= 2 && ty /= Substructural
                     then Q.push queue bigV
                     else queue

rep :: Var s -> ST s (Var s)
rep v = do
    r <- findRep v
    setRep r v
  where
    findRep v'@(Var _ _ _ repRef _ _ _) = do
        r <- readSTRef repRef
        case r of
            Just r' -> findRep r'
            Nothing -> return v'
    setRep r (Var _ _ _ repRef _ _ _) = do
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

commonFrontier :: ST s ID -> Queue (Var s) -> V.Vector (RNode s) -> ST s (Either (Term s, Term s) (Queue (Var s)))
commonFrontier unique queue t_list = do
    t@(Term _ sym _ _) <- fromRTerm $ t_list V.! 0
    ts <- V.mapM fromRTerm t_list
    case checkEqual t ts of
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
                        Right <$> if all (matchingAttrib a) terms'
                                      then return queue'
                                      else genSubVar queue' i $ t:terms
                    _ -> commonFrontier unique queue' . V.fromList $ t:terms
            (terms, (j, var):vars) -> do
                swapChild i j
                v <- rep =<< fromRVar var
                queue''  <- foldM (processVars  v) queue' $ map snd vars
                queue''' <- foldM (processTerms v) queue'' terms
                return $ Right queue'''
    ithChildren i = V.forM t_list $ \t -> do
        (Term _ _ children _) <- fromRTerm t
        return $ children SL.! i
    genSubVar queue' i terms = do
        v <- mkRVar unique "attr" Substructural
        v' <- fromRVar v
        queue'' <- foldM (`add` v') queue' terms
        let t = t_list V.! 0
        Term ident sym cs stage <- fromRTerm t
        replaceNode t . Left $ Term ident sym (SL.update i v cs) stage
        return queue''
    swapChild i j = do
        let t0 = t_list V.! 0
            tj = t_list V.! j
        Term ident0 sym0 c0 stage0 <- fromRTerm t0
        Term identj symj cj stagej <- fromRTerm tj
        replaceNode t0 . Left $ Term ident0 sym0 (SL.update i (cj SL.! i) c0) stage0
        replaceNode tj . Left $ Term identj symj (SL.update i (c0 SL.! i) cj) stagej
    processVars v@(Var ident _ _ _ _ _ _) queue' var = do
        v2@(Var ident2 _ _ _ _ _ _) <- rep =<< fromRVar var
        if ident /= ident2
            then merge queue' v v2
            else return queue'
    processTerms v queue' = add queue' v

unify :: ST s ID -> V.Vector (RNode s) -> ST s (Maybe (Term s, Term s))
unify unique t_list = do
    res <- commonFrontier unique Q.empty t_list
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ Q.pop queue
  where
    go Nothing = return Nothing
    go (Just (Var _ _ _ _ termsRef _ _, queue')) = do
        terms <- readSTRef termsRef
        if TL.size terms < 2
            then go $ Q.pop queue'
            else do
                let t = TL.toVector terms
                writeSTRef termsRef . TL.singleton $ t V.! 0
                res <- commonFrontier unique queue' t
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
    go (Right (Var  ident _ _ _ _ _ _)) (Right (Var  ident' _ _ _ _ _ _)) | ident == ident' = return a
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
                    mapM_ (deloop unique) $ SL.toList children
                    case sym of
                        Or -> do
                            [a, b] <- mapM derefVar $ SL.toList children
                            replaceNode node =<< fromRNode =<< simplifyOr (const . const $ return node) a b
                        _  -> return ()
                    writeSTRef stageRef Delooped
                DeloopSeen -> case sym of
                                  Or -> replaceNode node =<< Left <$> mkTerm unique (Attrib False) []
                                  _  -> return ()
                _ -> return ()
        Right var -> do
            v@(Var _ sym tyRef _ terms _ _) <- rep var
            ty <- readSTRef tyRef
            ts <- TL.toVector <$> readSTRef terms
            V.mapM_ (deloop unique) ts
            ts' <- TL.toVector <$> readSTRef terms
            (terms', vars) <- V.foldM splitVars ([], []) ts'
            terms'' <- mapM fromRTerm terms'
            writeSTRef terms =<< case terms'' of
                                     [] -> case ty of
                                               Structural    -> return TL.empty
                                               Substructural -> TL.singleton <$> mkRTerm unique (Attrib False) []
                                               Void          -> return TL.empty
                                     [_] -> return . TL.singleton $ head terms'
                                     Term _ (Attrib a) _ _:_ | all (matchingAttrib a) terms'' -> return . TL.singleton $ head terms'
                                     _ -> error $ "variable " ++ sym ++ " has multiple values "
            mapM_ (mergeVar v) vars
  where
    derefVar n = do
        n' <- fromRNode n
        case n' of
            Left _ -> return n
            Right v -> do
                Var _ _ _ _ terms _ _ <- rep v
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
            vr@(Var ident _ _ _ _ _ _) <- rep v
            vr'@(Var ident' _ _ _ _ _ _) <- rep v'
            when (ident /= ident') . void $ merge Q.empty vr vr'

substitute :: RNode s -> ST s ()
substitute term = do
    Term _ _ children stageRef <- fromRTerm term
    stage <- readSTRef stageRef
    unless (stage == Substituted) $ do
        writeSTRef stageRef Substituted
        mapM_ go . zip [0..] $ SL.toList children
  where
    go (i, node) = do
        node' <- fromRNode node
        case node' of
            Left  _   -> substitute node
            Right var -> do
                Term ident sym children stage <- fromRTerm term
                Var _ _ tyRef _ terms _ _ <- rep var
                ty <- readSTRef tyRef
                ts <- TL.toVector <$> readSTRef terms
                when (ty /= Void && V.length ts == 1) . replaceNode term . Left $ Term ident sym (SL.update i (ts V.! 0) children) stage
                V.mapM_ substitute ts

purify :: H.HashTable s Int T.Type -> Term s -> ST s T.Type
purify seen (Term ident Attribs children _) = do
    purified <- H.lookup seen ident
    case purified of
        Just ty -> return ty
        Nothing -> do
            node <- fromRNode $ children SL.! 0
            Term _ (Attrib k) _ _ <- fromRTerm $ children SL.! 1
            Term _ (Attrib f) _ _ <- fromRTerm $ children SL.! 2
            let tType = T.Type ident k f
            case node of
                Left (Term _ sym children' _) -> do
                    rec let ty = tType $ rawType sym cs
                        H.insert seen ident ty
                        cs <- mapM (purify seen <=< fromRTerm) $ SL.toList children'
                    return ty
                Right var -> do
                    Var ident' _ tyRef _ terms _ _ <- rep var
                    vty <- readSTRef tyRef
                    case vty of
                        Void -> do
                            ts <- TL.toVector <$> readSTRef terms
                            rec let ty = tType . T.Void $ T.Type ident' k f v
                                H.insert seen ident ty
                                v <- if V.null ts
                                         then return T.Opaque
                                         else do
                                             Term _ sym children' _ <- fromRTerm (ts V.! 0)
                                             cs <- mapM (purify seen <=< fromRTerm) $ SL.toList children'
                                             return $ rawType sym cs
                            return ty
                        _ -> do
                            let ty = tType T.Opaque
                            H.insert seen ident ty
                            return ty
  where
    rawType Product       [a, b] = T.Product a b
    rawType Sum           [a, b] = T.Sum     a b
    rawType Block         [a, b] = T.Block   a b
    rawType Num           _      = T.Num
    rawType Unit          _      = T.Unit
    rawType (Sealed seal) [a]    = T.Sealed seal a
    rawType _             _      = error "Illegal term type in purify"
purify _ (Term _ sym _ _) = error $ "Attempted to purify non-Attribs term " ++ show sym

mkAttribTerm :: ST s ID -> Symbol -> Either [RNode s] Bool -> Either [RNode s] Bool -> [RNode s] -> ST s (RNode s)
mkAttribTerm unique sym relevant affine children = do
    t <- mkRTerm' sym children
    false <- mkRTerm' (Attrib False) []
    k <- mkAttrib false relevant "k" fst
    f <- mkAttrib false affine   "f" snd
    mkRTerm' Attribs [t, k, f]
  where
    mkRTerm' = mkRTerm unique
    mkAttrib false attrib name extract = case attrib of
              Left  [] -> mkRVar unique name Substructural
              Left  as -> foldM (simplifyOr mkOr) false =<< map extract <$> mapM getAttribs as
              Right a  -> mkRTerm' (Attrib a) []
    mkOr a b = mkRTerm' Or [a, b]

mkAttribVar :: ST s ID -> String -> VarType -> Maybe Bool -> Maybe Bool -> ST s (RNode s)
mkAttribVar unique name ty relevant affine = do
    v <- mkRVar unique name ty
    k <- mkAttrib relevant "_k"
    f <- mkAttrib affine   "_f"
    mkRTerm unique Attribs [v, k, f]
  where
    mkAttrib (Just a) _      = mkRTerm unique (Attrib a) []
    mkAttrib Nothing  suffix = mkRVar unique (name ++ suffix) Substructural

type RNodeIL s = IL.InterList (RNode s)

opType :: ST s ID -> Op (RNodeIL s) -> ST s (RNode s, RNode s)
opType unique = flip evalStateT (False, M.empty) . blockOrOp
  where
    mkRTerm' sym children = lift $ mkRTerm unique sym children
    mkAttribTerm' sym children = do
        onRight <- gets fst
        lift $ if onRight
                   then mkAttribTerm unique sym (Left children) (Left children) children
                   else mkAttribTerm unique sym (Left [])       (Left [])       children
    eval x' = lift $ evalStateT x' (False, M.empty)
    seq2 action a' b' = do
        a'' <- a'
        b'' <- b'
        action a'' b''

    a' ~> b' = do
        modify $ \(_, vars) -> (False, vars)
        a'' <- a'
        modify $ \(_, vars) -> (True, vars)
        b'' <- b'
        return (a'', b'')
    (.*) = seq2 $ \a' b' -> mkAttribTerm' Product [a', b']
    (.+) = seq2 $ \a' b' -> mkAttribTerm' Sum [a', b']
    infixr 1 ~>
    infixr 7 .*
    infixr 6 .+
    mkBlock rel aff = seq2 $ \a' b' -> lift $ mkAttribTerm unique Block rel aff [a', b']
    mkBlockAny = seq2 $ \a' b' -> mkAttribTerm' Block [a', b']
    mkBlockNew = mkBlock (Right False) (Right False)
    unit = lift $ mkAttribTerm unique Unit (Right False) (Right False) []
    num  = lift $ mkAttribTerm unique Num  (Right False) (Right False) []
    sealed seal v = do
        v' <- v
        mkAttribTerm' (Sealed seal) [v']
    void' = lift $ mkAttribVar unique "0" Void Nothing Nothing

    var  v = var' v Nothing Nothing
    var' v relevant affine = do
        (onRight, vars) <- get
        case M.lookup v vars of
            Just v' -> return v'
            Nothing -> do
                v' <- lift $ mkAttribVar unique v Structural relevant affine
                put (onRight, M.insert v v' vars)
                return v'

    a = var "a"
    b = var "b"
    c = var "c"
    d = var "d"
    e = var "e"
    s = var "s"
    x = var "x"
    xp = var "x'"
    y = var "y"
    z = var "z"
    xDrop = var' "x" (Just False) Nothing
    xCopy = var' "x" Nothing (Just False)

    mkText = do
        v <- eval $ var' "L" (Just False) (Just False)
        list <- eval $ num .* return v .+ unit
        Var _ _ _ _ termRef _ _ <- lift $ fromRVar =<< getType v
        lift $ writeSTRef termRef . TL.singleton =<< getType list
        return v
    
    opMark relevant = do
        b1 <- eval $ mkBlockAny x y
        t      <- lift $ getType b1
        (k, f) <- lift $ getAttribs b1
        true <- mkRTerm' (Attrib True) []
        b2   <- mkRTerm' Attribs $ if relevant
                                       then [t, true, f]
                                       else [t, k, true]
        return b1 .* e ~> return b2 .* e

    blockOrOp (LitBlock block) = do
        let tys = IL.outerList block
        s ~> mkBlockNew (return $ head tys) (return $ last tys) .* s
    blockOrOp (Op op') = op op'

    op (LitText _) = do
        text <- mkText
        e ~> return text .* e

    op AssocL = a .* b .* c ~> (a .* b) .* c
    op AssocR = (a .* b) .* c ~> a .* b .* c
    op Swap   = a .* b .* c ~> b .* a .* c
    op SwapD  = a .* b .* c .* d ~> a .* c .* b .* d
    op Intro1 = a ~> a .* unit
    op Elim1  = a .* unit ~> a
    op Drop   = xDrop .* e ~> e
    op Copy   = xCopy .* e ~> xCopy .* xCopy .* e

    op Apply   = mkBlockAny x xp .* x .* e ~> xp .* e
    op Compose = do
        x' <- eval x
        y' <- eval y
        z' <- eval z
        b1 <- eval $ mkBlockAny (return x') (return y')
        b2 <- eval $ mkBlockAny (return y') (return z')
        return b1 .* return b2 .* e ~> mkBlock (Left [b1, b2]) (Left [b1, b2]) (return x') (return z') .* e
    op Quote = x .* e ~> mkBlockAny s (x .* s) .* e
    op Relevant = opMark True
    op Affine = opMark False

    op IntroNum  = e ~> num .* e
    op (Digit _) = num .* e ~> num .* e

    op Add      = num .* num .* e ~> num .* e
    op Multiply = num .* num .* e ~> num .* e
    op Inverse  = num .* e ~> num .* e
    op Negate   = num .* e ~> num .* e
    op Divmod   = num .* num .* e ~> num .* num .* e

    op AssocLS = (a .+ b .+ c) .* e ~> ((a .+ b) .+ c) .* e
    op AssocRS = ((a .+ b) .+ c) .* e ~> (a .+ b .+ c) .* e
    op SwapS   = (a .+ b .+ c) .* e ~> (b .+ a .+ c) .* e
    op SwapDS  = (a .+ b .+ c .+ d) .* e ~> (a .+ c .+ b .+ d) .* e
    op Intro0  = a .* e ~> (a .+ void') .* e
    op Elim0   = (a .+ void') .* e ~> a .* e

    op CondApply = mkBlock (Left []) (Right False) x xp .* (x .+ y) .* e ~> (xp .+ y) .* e
    op Distrib   = a .* (b .+ c) .* e ~> (a .* b .+ a .* c) .* e
    op Factor    = (a .* b .+ c .* d) .* e ~> (a .+ c) .* (b .+ d) .* e
    op Merge     = do
        a' <- eval a
        b' <- eval b
        c' <- eval c
        Var _ _ _ _ _ mergesRef _ <- lift $ fromRVar =<< getType c'
        lift . writeSTRef mergesRef $ TL.singleton (a', b')
        (return a' .+ return b') .* e ~> return c' .* e
    op Assert    = (a .+ b) .* e ~> b .* e

    op Greater = num .* num .* e ~> (num .* num .+ num .* num) .* e

    op (Sealer   seal) = a ~> sealed seal a
    op (Unsealer seal) = sealed seal a ~> a

    op AssertEQ = a .* b .* e ~> a .* b .* e
    op DebugPrintRaw = a .* e ~> a .* e
    op DebugPrintText = do
        text <- mkText
        return text .* e ~> return text .* e

    op ApplyTail = error "To be removed."

unifyBlock :: ST s ID -> RawOp -> ST s (Either (Int, Term s, Term s, Term s, Term s) (Op (RNodeIL s)))
unifyBlock _      (Op op) = return . Right $ Op op
unifyBlock unique (LitBlock bops) = do
    btyOps <- unifyAll unique bops
    return $ case btyOps of
                 Left  err     -> Left err
                 Right btyOps' -> Right $ LitBlock btyOps'

unifyAll :: ST s ID -> [RawOp] -> ST s (Either (Int, Term s, Term s, Term s, Term s) (RNodeIL s (Op (RNodeIL s))))
unifyAll unique [] = Right . IL.empty <$> mkAttribVar unique "a" Structural (Just False) (Just False)
unifyAll unique (opcode:opcodes) = do
    op <- unifyBlock unique opcode
    case op of
        Left  err -> return $ Left err
        Right op' -> do
            (a, b) <- opType unique op'
            go 0 (IL.empty a) op' opcodes b
  where
    go i tyOps lop (rop:ops) a = do
        rop' <- unifyBlock unique rop
        case rop' of
            Left err -> return $ Left err
            Right rop'' -> do
                (b, a') <- opType unique rop''
                res <- unify unique $ V.fromList [a, b]
                case res of
                    Just (x, y) -> do
                        a'' <- fromRTerm a
                        b'  <- fromRTerm b
                        return $ Left (i, a'', b', x, y)
                    Nothing -> go (i + 1) (IL.cons a lop tyOps) rop'' ops a'
    go _ tyOps op [] a = return . Right . IL.reverse $ IL.cons a op tyOps

inferTypes :: Mode -> [TIStage] -> [RawOp] -> Producer (TIStage, String) (ST s) (Either (Int, String) [T.Type])
inferTypes mode logStages ops = do
    counter <- lift $ newSTRef (0 :: Int)
    let unique = do
            modifySTRef' counter (+1)
            readSTRef counter
    let writeGraph stage xs =
            when (stage `elem` logStages) $ do
                rootID <- lift unique
                let root = Node rootID (show stage) . zip (map (('#':) . show) [1 :: Int ..]) $ map (toNode mode <=< fromRTerm) xs
                graph <- lift $ showGraph $ Graph "node" "" [] [root]
                yield (stage, graph)
        errorTerm prefix label x y = lift $ do
            x' <- toNode mode x
            y' <- toNode mode y
            return $ Graph prefix label [] [x', y']
    res <- lift $ unifyAll unique ops
    case res of
        Left (i, a, b, x, y) -> do
            inner <- errorTerm "inner" "Could not unify:" x y
            outer <- errorTerm "outer" "While trying to unify:" a b
            graph <- lift . showGraph $ Graph "" ("Unification failure at opcode index " ++ show i) [inner, outer] []
            return $ Left (i, graph)
        Right tyOps -> do
            let tys = IL.outerList tyOps
            writeGraph TIUnified tys
            lift $ mapM_ (deloop unique) tys
            writeGraph TIResolved tys
            lift $ mapM_ substitute tys
            writeGraph TISubstituted tys
            seen <- lift H.new
            pureExprs <- mapM (lift . (purify seen <=< fromRTerm)) tys
            return $ Right pureExprs
