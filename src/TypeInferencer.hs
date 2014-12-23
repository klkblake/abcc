{-# LANGUAGE TypeFamilies, TupleSections, FlexibleInstances, FlexibleContexts, RecursiveDo, TemplateHaskell, Rank2Types, BangPatterns #-}
module TypeInferencer
    ( inferTypes
    , TIStage (..)
    ) where

-- Implementation of the algorithm described in "Efficient Unification over
-- Infinite Types" by Joxan Jaffar. Runs in O(n*F(n)) time, where F(n) is
-- the functional inverse of the Ackermann function. Be careful to preserve
-- this time complexity!

import Prelude hiding (read, all, mapM_, elem, concat, concatMap)

import Control.Applicative
import Control.Lens hiding (op, children)
import Control.Monad hiding (mapM_)
import Control.Monad.ST
import Control.Monad.State hiding (mapM_, modify)
import qualified Control.Monad.State as State
import Control.Monad.Writer hiding (Product, Sum, mapM_)
import Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Pipes hiding ((~>))

import Op
import qualified Type as T

import GraphViz hiding (Node)
import qualified GraphViz as GV
import qualified InterList as IL
import ShortList (ShortList)
import qualified ShortList as SL
import TreeList (TreeList)
import qualified TreeList as TL
import Queue (Queue)
import qualified Queue as Q

data TIStage = TIUnified
             deriving (Eq, Enum, Bounded, Show)

data Symbol = Product
            | Sum
            | Block
            | Num
            | Unit
            | Sealed String
            deriving Eq

instance Show Symbol where
    show Product       = "*"
    show Sum           = "+"
    show Block         = "[ -> ]"
    show Num           = "N"
    show Unit          = "1"
    show (Sealed seal) = '{' : seal ++ "}"

arity :: Symbol -> Int
arity Product    = 2
arity Sum        = 2
arity Block      = 2
arity Num        = 0
arity Unit       = 0
arity (Sealed _) = 1

data VarType = Structural | Void deriving Eq

newtype RNode s = RNode (STRef s (Node s))

data Node s = Term { _nodeID   :: {-# UNPACK #-} !ID
                   , _purified ::                !(Maybe T.Type)
                   , _symbol   ::                !Symbol
                   , _children :: {-# UNPACK #-} !(ShortList (RNode s))
                   }
            | Var  { _nodeID   :: {-# UNPACK #-} !ID
                   , _purified ::                !(Maybe T.Type)
                   , _name     ::                 String
                   , _varType  ::                !VarType
                   , _repVar   ::                !(Maybe (RNode s))
                   , _terms    ::                !(TreeList (RNode s))
                   , _varCount :: {-# UNPACK #-} !Int
                   }

makeLenses ''Node

nodeLabel :: Node s -> String
nodeLabel term@Term {} = show $ term^?!symbol
nodeLabel var@Var {}   = (var^?!name) ++ " (" ++ show (var^?!varCount) ++ ")" ++ case var^?!varType of
                                                                                     Structural    -> ""
                                                                                     Void          -> " void"

type ToNetlistT s = WriterT ([GV.Node], [GV.Edge]) (StateT (IM.IntMap ID) (ST s))

lift2 :: ST s a -> ToNetlistT s a
lift2 = lift . lift

ifUnseen :: Node s -> ToNetlistT s (ID, Bool) -> ToNetlistT s (ID, Bool)
ifUnseen node action = do
    let ident = node^.nodeID
    seen <- get
    case IM.lookup ident seen of
        Just ident' -> return (ident', False)
        Nothing -> do
            put $ IM.insert ident ident seen
            action

mkEdge :: ID -> ID -> String -> Edge
mkEdge ident ident' label = GV.Edge ident ident' Nothing Nothing label

toNetlistLabel :: GV.Mode -> Node s -> String -> ToNetlistT s (ID, Bool)
toNetlistLabel mode term@Term {} label = ifUnseen term toNetlist'
  where
    ident = term^.nodeID
    toNetlist' = do
        cs <- lift2 $ childList' term
        subNodes <- mapM (toNetlist mode) cs
        let thisNode = GV.Node ident label
            thisEdges = zipWith (mkEdge ident) subNodes (map (('#':) . show) [1 :: Int ..])
        tell ([thisNode], thisEdges)
        return (ident, True)

toNetlistLabel mode var@Var {} label = ifUnseen var $ do
    case mode of
        Verbose -> toNetlist'
        Compact -> do
            case var^?!repVar of
                Just r -> do
                    rec State.modify $ IM.insert ident ident'
                        n <- lift2 $ fromRNode r
                        (ident', new) <- toNetlistLabel mode n $ nodeLabel n
                    return (ident', new)
                Nothing -> toNetlist'
  where
    ident = var^.nodeID
    toNetlist' = do
        repEdge <- case var^?!repVar of
                       Just r -> do
                           r' <- lift2 $ fromRNode r
                           ident' <- toNetlist mode r'
                           return [mkEdge ident ident' "rep"]
                       Nothing -> return []
        ts <- lift2 $ mapM fromRNode . V.toList . TL.toVector $ var^?!terms
        ts' <- mapM (toNetlist mode) ts
        let thisNode = GV.Node ident label
            termEdges = zipWith (\label' t -> mkEdge ident t label') numbers ts'
        tell ([thisNode], repEdge ++ termEdges)
        return (ident, True)
    numbers = map (('#':) . show) [1 :: Int ..]

toNetlist :: GV.Mode -> Node s -> ToNetlistT s ID
toNetlist mode node = fst <$> toNetlistLabel mode node (nodeLabel node)

mkTerm :: ST s ID -> Symbol -> [RNode s] -> ST s (Node s)
mkTerm unique sym cs = do
    ident <- unique
    return . Term ident Nothing sym $ SL.fromList cs

mkVar :: ST s ID -> String -> VarType -> ST s (Node s)
mkVar unique v ty = do
    ident <- unique
    return $ Var ident Nothing v ty Nothing TL.empty 1

toRNode :: Node s -> ST s (RNode s)
toRNode node = RNode <$> newSTRef node

mkRTerm :: ST s ID -> Symbol -> [RNode s] -> ST s (RNode s)
mkRTerm unique sym cs = toRNode =<< mkTerm unique sym cs

mkRVar :: ST s ID -> String -> VarType -> ST s (RNode s)
mkRVar unique v ty = toRNode =<< mkVar unique v ty

fromRNode :: RNode s -> ST s (Node s)
fromRNode (RNode ref) = readSTRef ref

replaceNode :: RNode s -> Node s -> ST s ()
replaceNode (RNode ref) !n = writeSTRef ref n

childList :: Node s -> [RNode s]
childList t = t^..children.traverse

childList' :: Node s -> ST s [Node s]
childList' = mapM fromRNode . childList

updateChild :: RNode s -> Node s -> Int -> RNode s -> ST s ()
updateChild n t i c = replaceNode n $ children %~ SL.update i c $ t
{-# INLINE updateChild #-}

add :: Queue (RNode s) -> RNode s -> RNode s -> ST s (Queue (RNode s))
add queue v t = do
    v' <- fromRNode v
    replaceNode v $ terms %~ TL.cons t $ v'
    return $ if TL.size (v'^?!terms) == 1
                 then Q.push queue v
                 else queue

merge :: Queue (RNode s) -> RNode s -> RNode s -> ST s (Queue (RNode s))
merge queue n1 n2 = do
    v1 <- fromRNode n1
    v2 <- fromRNode n2
    let r1 = v1^?!varCount
        r2 = v2^?!varCount
        vc = r1 + r2
    if r1 >= r2
        then go vc n1 n2 v1 v2
        else go vc n2 n1 v2 v1
  where
    go vc bigNode node bigV v = do
        let ty = v^?!varType
            bty = if ty == Void then Void else bigV^?!varType
            bigTerms  = bigV^?!terms
            bigTerms' = TL.concat bigTerms $ v^?!terms
        replaceNode bigNode $ varType  .~ bty
                            $ terms    .~ bigTerms'
                            $ varCount .~ vc
                            $ bigV
        replaceNode node $ repVar   .~ Just bigNode
                         $ terms    .~ TL.empty
                         $ varCount .~ 0
                         $ v
        return $ if TL.size bigTerms <= 1 && TL.size bigTerms' >= 2
                     then Q.push queue bigNode
                     else queue

rep :: RNode s -> ST s (RNode s)
rep v = do
    r <- findRep v
    setRep r v
  where
    findRep n = do
        v' <- fromRNode n
        case v'^?!repVar of
            Just r' -> findRep r'
            Nothing -> return n
    setRep r n = do
        v' <- fromRNode n
        case v'^?!repVar of
            Just r'' -> do
                replaceNode n $ repVar .~ Just r $ v'
                setRep r r''
            Nothing  -> return r

commonFrontier :: ST s ID -> Queue (RNode s) -> V.Vector (RNode s) -> ST s (Either (Node s, Node s) (Queue (RNode s)))
commonFrontier unique queue t_list = do
    t <- fromRNode $ t_list V.! 0
    ts <- V.mapM fromRNode t_list
    if checkIdentical t ts
        then return $ Right queue
        else case checkEqual t ts of
                 Just err -> return $ Left err
                 Nothing  -> foldM goChild (Right queue) [0 .. arity (t^?!symbol) - 1]
  where
    checkIdentical t ts = V.all (\t' -> t^.nodeID == t'^.nodeID) ts
    checkEqual t ts =
        let diffs = V.filter (\t' -> t^?!symbol /= t'^?!symbol) ts
        in if V.null diffs
               then Nothing
               else Just (t, diffs V.! 0)
    goChild (Left err) _ = return $ Left err
    goChild (Right queue') i = do
        (ts, vs, j) <- splitIthChildren i
        case (V.null ts, V.null vs) of
            (False, True) -> do
                commonFrontier unique queue' ts
            (_, False) -> do
                swapChild i j
                v <- rep (vs V.! 0)
                queue''  <- V.foldM (processVars  v) queue' $ V.slice 1 (V.length vs - 1) vs
                queue''' <- V.foldM (processTerms v) queue'' ts
                return $ Right queue'''
            (True, True) -> error "commonFrontier called on empty term list"
    splitIthChildren i = do
        let len = V.length t_list
        v <- MV.new len
        splitIthChildren' v 0 0 0 $ len - 1
      where
        splitIthChildren' v vi j l r | l > r     = do v' <- V.unsafeFreeze v
                                                      return (V.unsafeSlice 0 l v', V.unsafeSlice l (V.length v' - l) v', vi)
                                     | otherwise = do
            cs <- fromJust . preview children <$> fromRNode (t_list V.! j)
            let node = cs SL.! i
            node' <- fromRNode node
            case node' of
                Term {} -> do MV.write v l node
                              splitIthChildren' v vi (j + 1) (l + 1) r
                Var  {} -> do MV.write v r node
                              splitIthChildren' v j (j + 1) l (r - 1)
    swapChild i j = do
        let term0 = t_list V.! 0
            termj = t_list V.! j
        t0 <- fromRNode term0
        tj <- fromRNode termj
        updateChild term0 t0 i $ (tj^?!children) SL.! i
        updateChild termj tj i $ (t0^?!children) SL.! i
    processVars v queue' var = do
        ident <- view nodeID <$> fromRNode v
        v2 <- rep var
        ident2 <- view nodeID <$> fromRNode v2
        if ident /= ident2
            then merge queue' v v2
            else return queue'
    processTerms v queue' = add queue' v

unify :: ST s ID -> V.Vector (RNode s) -> ST s (Maybe (Node s, Node s))
unify unique t_list = do
    [a, b] <- mapM fromRNode $ V.toList t_list
    res <- case (a, b) of
               (Term {}, Term {}) -> commonFrontier unique Q.empty t_list
               (Var  {}, Term {}) -> do
                   a' <- rep $ t_list V.! 0
                   Right <$> add Q.empty a' (t_list V.! 1)
               (Term {}, Var  {}) -> do
                   b' <- rep $ t_list V.! 1
                   Right <$> add Q.empty b' (t_list V.! 0)
               (Var  {}, Var  {}) -> do
                   a' <- rep $ t_list V.! 0
                   b' <- rep $ t_list V.! 1
                   identA <- view nodeID <$> fromRNode a'
                   identB <- view nodeID <$> fromRNode b'
                   Right <$> if identA /= identB
                                 then merge Q.empty (t_list V.! 0) (t_list V.! 1)
                                 else return Q.empty
    case res of
        Left  err   -> return $ Just err
        Right queue -> go $ Q.pop queue
  where
    go Nothing = return Nothing
    go (Just (v, queue')) = do
        v' <- fromRNode v
        let ts = v'^?!terms
        if TL.size ts < 2
            then go $ Q.pop queue'
            else do
                let t = TL.toVector ts
                replaceNode v $ terms .~ TL.singleton (t V.! 0) $ v'
                res <- commonFrontier unique queue' t
                case res of
                    Left  err     -> return $ Just err
                    Right queue'' -> go $ Q.pop queue''

purify :: STRef s ID -> RNode s -> ST s T.Type
purify nextID node = do
    node' <- fromRNode node
    case node'^.purified of
        Just ty -> return ty
        Nothing -> do
            let tType = T.Type (node'^.nodeID) False False
            case node' of
                t@Term {} -> do
                    rec let rty = rawType (t^?!symbol) cs
                            ty = tType rty
                        replaceNode node $ purified .~ Just ty $ node'
                        cs <- mapM (purify nextID) $ childList t
                    rty `seq` return ty
                Var {} -> do
                    v <- case node'^?!repVar of
                             Just r  -> fromRNode =<< rep r
                             Nothing -> return node'
                    let ts = TL.toVector $ v^?!terms
                    case v^?!varType of
                        Void -> do
                            rec let ty = tType . T.Void $ T.Type (v^.nodeID) False False ty'
                                replaceNode node $ purified .~ Just ty $ node'
                                ty' <- if V.null ts
                                           then T.Opaque <$> getIdent
                                           else do
                                               t' <- fromRNode (ts V.! 0)
                                               cs <- mapM (purify nextID) $ childList t'
                                               return $ rawType (t'^?!symbol) cs
                            return ty
                        _ -> do
                            ty <- if V.null ts
                                      then do
                                          ident' <- getIdent
                                          let opaque = T.Opaque ident'
                                          return $ opaque `seq` tType opaque
                                      else purify nextID $ ts V.! 0
                            replaceNode node $ purified .~ Just ty $ node'
                            return ty
  where
    getIdent = do
        ident'' <- readSTRef nextID
        writeSTRef nextID $! ident'' + 1
        return ident''
    rawType Product       [a, b] = T.Product a b
    rawType Sum           [a, b] = T.Sum     a b
    rawType Block         [a, b] = T.Block   a b
    rawType Num           _      = T.Num
    rawType Unit          _      = T.Unit
    rawType (Sealed seal) [a]    = T.Sealed seal a
    rawType _             _      = error "Illegal term type in purify"

type RNodeIL s = IL.InterList (RNode s)

opType :: ST s ID -> Op (RNodeIL s) -> ST s (RNode s, RNode s, Maybe (RNode s, RNode s, RNode s))
opType unique = flip evalStateT M.empty . blockOrOp
  where
    mkRTerm' sym cs = lift $ mkRTerm unique sym cs
    eval x' = lift $ evalStateT x' M.empty
    seq2 action a' b' = do
        a'' <- a'
        b'' <- b'
        action a'' b''

    (~>) = seq2 $ \a' b' -> return (a', b', Nothing)
    (.*) = seq2 $ \a' b' -> mkRTerm' Product [a', b']
    (.+) = seq2 $ \a' b' -> mkRTerm' Sum [a', b']
    infixr 1 ~>
    infixr 7 .*
    infixr 6 .+
    block = seq2 $ \a' b' -> mkRTerm' Block [a', b']
    unit = mkRTerm' Unit []
    num  = mkRTerm' Num  []
    sealed seal v = do
        v' <- v
        mkRTerm' (Sealed seal) [v']
    void' = lift $ mkRVar unique "0" Void

    var v = do
        vars <- get
        case M.lookup v vars of
            Just v' -> return v'
            Nothing -> do
                v' <- lift $ mkRVar unique v Structural
                put $ M.insert v v' vars
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

    mkText = do
        v <- eval $ var "L"
        list <- eval $ num .* return v .+ unit
        lift $ do
            v' <- fromRNode v
            replaceNode v $ terms .~ TL.singleton list $ v'
        return v

    blockOrOp (LitBlock block') = do
        let tys = IL.outerList block'
        s ~> block (return $ head tys) (return $ last tys) .* s
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
    op Drop   = x .* e ~> e
    op Copy   = x .* e ~> x .* x .* e

    op Apply    = block x xp .* x .* e ~> xp .* e
    op Compose  = block x y .* block y z .* e ~> block x z .* e
    op Quote    = x .* e ~> block s (x .* s) .* e
    op Relevant = block x y .* e ~> block x y .* e
    op Affine   = block x y .* e ~> block x y .* e

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

    op CondApply = block x xp .* (x .+ y) .* e ~> (xp .+ y) .* e
    op Distrib   = a .* (b .+ c) .* e ~> (a .* b .+ a .* c) .* e
    op Factor    = (a .* b .+ c .* d) .* e ~> (a .+ c) .* (b .+ d) .* e
    op Merge     = do
        a' <- eval a
        b' <- eval b
        c' <- eval c
        e' <- eval e
        left  <- (return a' .+ return b') .* return e'
        right <- return c' .* return e'
        return (left, right, Just (a', b', c'))
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

unifyBlock :: ST s ID -> [(RNode s, RNode s, RNode s)] -> RawOp -> ST s (Either (Int, Node s, Node s, Node s, Node s) (Op (RNodeIL s), [(RNode s, RNode s, RNode s)]))
unifyBlock _      merges (Op op) = return $ Right (Op op, merges)
unifyBlock unique merges (LitBlock bops) = do
    btyOps <- unifyAll unique merges bops
    return $ case btyOps of
                 Left  err               -> Left err
                 Right (btyOps', merges') -> Right (LitBlock btyOps', merges')

unifyAll :: ST s ID -> [(RNode s, RNode s, RNode s)] -> [RawOp] -> ST s (Either (Int, Node s, Node s, Node s, Node s) (RNodeIL s (Op (RNodeIL s)), [(RNode s, RNode s, RNode s)]))
unifyAll unique merges [] = do
    a <- mkRVar unique "a" Structural
    return $ Right (IL.empty a, merges)
unifyAll unique merges (opcode:opcodes) = do
    op <- unifyBlock unique merges opcode
    case op of
        Left  err -> return $ Left err
        Right (op', merges') -> do
            (a, b, mmerge) <- opType unique op'
            let merges'' = case mmerge of
                               Just m  -> m:merges'
                               Nothing -> merges'
            go 0 merges'' (IL.empty a) op' opcodes b
  where
    go i merges' tyOps lop (rop:ops) a = do
        rop' <- unifyBlock unique merges' rop
        case rop' of
            Left err -> return $ Left err
            Right (rop'', merges'') -> do
                (b, a', mmerge) <- opType unique rop''
                res <- unify unique $ V.fromList [a, b]
                case res of
                    Just (x, y) -> do
                        a'' <- fromRNode a
                        b'  <- fromRNode b
                        return $ Left (i, a'', b', x, y)
                    Nothing -> let merges''' = case mmerge of
                                                  Just m -> m:merges''
                                                  Nothing -> merges''
                               in go (i + 1) merges''' (IL.cons a lop tyOps) rop'' ops a'
    go _ merges' tyOps op [] a = return $ Right (IL.reverse $ IL.cons a op tyOps, merges')

mapMTyOps :: (Applicative m, Monad m) => (a -> m b) -> (FlatOp -> m FlatOp) -> IL.InterList a (Op (IL.InterList a)) -> m (IL.InterList b (Op (IL.InterList b)))
mapMTyOps f g = IL.mapM f op
  where
    op (LitBlock tyOps) = do
        tyOps' <- mapMTyOps f g tyOps
        return . LitBlock $! tyOps'
    op (Op op') = Op <$> g op'

inferTypes :: Mode -> [TIStage] -> [RawOp] -> Producer (TIStage, String) (ST s) (Either (Int, String) (IL.InterList T.Type TyOp))
inferTypes mode logStages ops = do
    counter <- lift $ newSTRef (0 :: Int)
    let unique = do
            modifySTRef' counter (+1)
            readSTRef counter
    let writeGraph stage' xs =
            when (stage' `elem` logStages) $ do
                rootID <- lift unique
                let root = GV.Node rootID (show stage')
                ns <- lift $ mapM fromRNode xs
                (idents, netList) <- lift $ evalStateT (runWriterT $ mapM (toNetlist mode) ns) IM.empty
                let edges = zipWith (mkEdge rootID) idents $ map (('#':) . show) [1 :: Int ..]
                    graph = showGraph $ Graph "node" "" [] (root:fst netList) (edges ++ snd netList)
                yield (stage', graph "")
        errorTerm prefix label x y = lift $ do
            netList <- evalStateT (execWriterT $ mapM (toNetlist mode) [x, y]) IM.empty
            return $ Graph prefix label [] (fst netList) (snd netList)
    res <- lift $ unifyAll unique [] ops
    case res of
        Left (i, a, b, x, y) -> do
            inner <- errorTerm "inner" "Could not unify:" x y
            outer <- errorTerm "outer" "While trying to unify:" a b
            let graph = showGraph $ Graph "" ("Unification failure at opcode index " ++ show i) [inner, outer] [] []
            return $ Left (i, graph "")
        Right (tyOps, _) -> do
            let tys = IL.outerList tyOps
            writeGraph TIUnified tys
            tyOps' <- lift $ do
                nextID <- newSTRef 0
                mapMTyOps (purify nextID) pure tyOps
            return $ Right tyOps'
