{-# LANGUAGE TypeFamilies, TupleSections #-}
module GraphViz
    ( ID
    , Mode (..)
    , Node (..)
    , Edge (..)
    , Graph (..)
    , showGraph
    , showSubgraph
    ) where

import Numeric

type ID = Int

data Mode = Verbose | Compact

data Node = Node ID String

data Edge = Edge ID ID String

data Graph = Graph String String [Graph] [Node] [Edge]

showID :: String -> ID -> ShowS
showID prefix ident = showString prefix . showChar '_' . showInt ident

showLabel :: String -> ShowS
showLabel label = showString " [label=\"" . showString label . showString "\"]"

showNode :: String -> Node -> ShowS
showNode prefix (Node ident label) = showID prefix ident . showLabel label . showChar '\n'

showEdge :: String -> Edge -> ShowS
showEdge prefix (Edge from to label) = showID prefix from . showString " -> " . showID prefix to . showLabel label . showChar '\n'

composeMap :: (a -> b -> b) -> [a] -> b -> b
composeMap f xs = foldr (.) id $ map f xs

showSubgraph :: Graph -> ShowS
showSubgraph (Graph prefix label graphs nodes edges) =
    let gs = composeMap showSubgraph      graphs
        ns = composeMap (showNode prefix) nodes
        es = composeMap (showEdge prefix) edges
    in showString "subgraph cluster_" . showString prefix . showString "{\n" . gs . ns . es . showString "label=\"" . showString label . showString "\"\nlabelloc=top\nlabeljust=center\n}"

showGraph :: Graph -> ShowS
showGraph (Graph prefix label graphs nodes edges) =
    let gs = composeMap showSubgraph      graphs
        ns = composeMap (showNode prefix) nodes
        es = composeMap (showEdge prefix) edges
    in showString "digraph {\n" . gs . ns . es . showString "label=\"" . showString label . showString "\"\nlabelloc=top\nlabeljust=center\n}"
