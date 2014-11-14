{-# LANGUAGE TypeFamilies, TupleSections #-}
module GraphViz
    ( ID
    , Node (..)
    , GraphViz (..)
    , showSubgraph
    ) where

import Control.Applicative
import Control.Monad.ST
import qualified Data.IntSet as IS
import Data.List
import Numeric

import qualified Queue as Q

type ID = Int

showID :: String -> ID -> ShowS
showID prefix ident = showString prefix . showChar '_' . showInt ident

showLabel :: String -> ShowS
showLabel label = showString " [label=\"" . showString label . showString "\"]"

showNode :: String -> ID -> String -> ShowS
showNode prefix ident label = showID prefix ident . showLabel label . showChar '\n'

showEdge :: String -> ID -> ID -> String -> ShowS
showEdge prefix from to label = showID prefix from . showString " -> " . showID prefix to . showLabel label . showChar '\n'

data Node s = Node ID String [(String, ST s (Node s))]

class GraphViz g where
    type State g
    toNode :: g -> ST (State g) (Node (State g))

instance (GraphViz a, GraphViz b, State a ~ State b) => GraphViz (Either a b) where
    type State (Either a b) = State a
    toNode (Left  x) = toNode x
    toNode (Right y) = toNode y

showSubgraph :: GraphViz g => String -> g -> ST (State g) String
showSubgraph name g = do
    node <- toNode g
    ($ "") . (showString "subgraph {\n" .) . (. showString "\n}") <$> go IS.empty (Just (node, Q.empty))
  where
    go _ Nothing = return id
    go seen (Just (Node ident _ _, queue)) | IS.member ident seen = go seen $ Q.pop queue
    go seen (Just (Node ident label children, queue)) = do
        let seen' = IS.insert ident seen
        children' <- mapM (\(x, y) -> (x,) <$> y) children
        let chunk = showNode name ident label . foldr (\(l, Node ident' _ _) s -> showEdge name ident ident' l . s) id children'
        fmap (chunk .) . go seen' . Q.pop . foldl' Q.push queue $map snd children'
