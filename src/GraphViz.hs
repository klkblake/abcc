{-# LANGUAGE TypeFamilies, TupleSections #-}
module GraphViz
    ( ID
    , Mode (..)
    , Node (..)
    , Port (..)
    , Edge (..)
    , Graph (..)
    , showGraph
    , showSubgraph
    ) where

import Data.Char
import Numeric

type ID = Int

data Mode = Verbose | Compact

data Node = Node ID String

data Port = N | NE | E | SE | S | SW | W | NW
          deriving Show

data Edge = Edge ID ID (Maybe Port) (Maybe Port) String

data Graph = Graph String String [Graph] [Node] [Edge]

showID :: String -> ID -> ShowS
showID prefix ident = showString prefix . showChar '_' . showInt ident

showIDPort :: String -> ID -> Maybe Port -> ShowS
showIDPort prefix ident Nothing = showID prefix ident
showIDPort prefix ident (Just port) = showID prefix ident . showChar ':' . showString (map toLower $ show port)

showLabel :: String -> ShowS
showLabel label = showString " [label=\"" . showString (concatMap quoted label) . showString "\"]"
  where
    quoted '\"' = "\\\""
    quoted c    = [c]

showNode :: String -> Node -> ShowS
showNode prefix (Node ident label) = showID prefix ident . showLabel label . showChar '\n'

showEdge :: String -> Edge -> ShowS
showEdge prefix (Edge from to fromPort toPort label) = showIDPort prefix from fromPort . showString " -> " . showIDPort prefix to toPort . showLabel label . showChar '\n'

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
