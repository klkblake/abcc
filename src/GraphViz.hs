{-# LANGUAGE TypeFamilies, TupleSections #-}
module GraphViz
    ( ID
    , Mode (..)
    , Node (..)
    , GraphViz (..)
    , Graph (..)
    , showGraph
    , showSubgraph
    ) where

import Control.Applicative
import Control.Monad.ST
import qualified Data.IntSet as IS
import Data.List
import Numeric

import qualified Queue as Q

type ID = Int

data Mode = Verbose | Compact

data Node s = Node ID String [(String, ST s (Node s))]

class GraphViz g where
    type State g
    toNode :: Mode -> g -> ST (State g) (Node (State g))

instance (GraphViz a, GraphViz b, State a ~ State b) => GraphViz (Either a b) where
    type State (Either a b) = State a
    toNode mode (Left  x) = toNode mode x
    toNode mode (Right y) = toNode mode y

data Graph s = Graph String String [Graph s] [Node s]

showID :: String -> ID -> ShowS
showID prefix ident = showString prefix . showChar '_' . showInt ident

showLabel :: String -> ShowS
showLabel label = showString " [label=\"" . showString label . showString "\"]"

showNode :: String -> ID -> String -> ShowS
showNode prefix ident label = showID prefix ident . showLabel label . showChar '\n'

showEdge :: String -> ID -> ID -> String -> ShowS
showEdge prefix from to label = showID prefix from . showString " -> " . showID prefix to . showLabel label . showChar '\n'

showNodes :: String -> Node s -> ST s String
showNodes prefix node = ($ "") <$> go IS.empty (Just (node, Q.empty))
  where
    go _ Nothing = return id
    go seen (Just (Node ident _ _, queue)) | IS.member ident seen = go seen $ Q.pop queue
    go seen (Just (Node ident label children, queue)) = do
        let seen' = IS.insert ident seen
        children' <- mapM (\(x, y) -> (x,) <$> y) children
        let chunk = showNode prefix ident label . foldr (\(l, Node ident' _ _) s -> showEdge prefix ident ident' l . s) id children'
        fmap (chunk .) . go seen' . Q.pop . foldl' Q.push queue $ map snd children'

showSubgraph :: Graph s -> ST s String
showSubgraph (Graph prefix label graphs nodes) = do
    body1 <- concat <$> mapM showSubgraph graphs
    body2 <- concat <$> mapM (showNodes prefix) nodes
    return . showString "subgraph cluster_" . showString prefix . showString "{\n" . showString body1 . showString body2 . showString "label=\"" $ showString label "\"\nlabelloc=top\nlabeljust=center\n}"

showGraph :: Graph s -> ST s String
showGraph (Graph prefix label graphs nodes) = do
    body1 <- concat <$> mapM showSubgraph graphs
    body2 <- concat <$> mapM (showNodes prefix) nodes
    return . showString "digraph {\n" . showString body1 . showString body2 . showString "label=\"" $ showString label "\"\nlabelloc=top\nlabeljust=center\n}"
