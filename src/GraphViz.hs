{-# LANGUAGE TypeFamilies, TupleSections #-}
module GraphViz
    ( ID
    , Mode (..)
    , Node (..)
    , GraphViz (..)
    , showGraph
    ) where

import Control.Applicative
import Control.Monad.ST
import qualified Data.IntSet as IS
import Data.List
import Numeric

import qualified Queue as Q

type ID = Int

showID :: ID -> ShowS
showID ident = showString "node_" . showInt ident

showLabel :: String -> ShowS
showLabel label = showString " [label=\"" . showString label . showString "\"]"

showNode :: ID -> String -> ShowS
showNode ident label = showID ident . showLabel label . showChar '\n'

showEdge :: ID -> ID -> String -> ShowS
showEdge from to label = showID from . showString " -> " . showID to . showLabel label . showChar '\n'

data Mode = Verbose | Compact

data Node s = Node ID String [(String, ST s (Node s))]

class GraphViz g where
    type State g
    toNode :: Mode -> g -> ST (State g) (Node (State g))

instance (GraphViz a, GraphViz b, State a ~ State b) => GraphViz (Either a b) where
    type State (Either a b) = State a
    toNode mode (Left  x) = toNode mode x
    toNode mode (Right y) = toNode mode y

showGraph :: GraphViz g => Mode -> g -> ST (State g) String
showGraph mode g = do
    node <- toNode mode g
    ($ "") . (showString "digraph {\n" .) . (. showChar '}') <$> go IS.empty (Just (node, Q.empty))
  where
    go _ Nothing = return id
    go seen (Just (Node ident _ _, queue)) | IS.member ident seen = go seen $ Q.pop queue
    go seen (Just (Node ident label children, queue)) = do
        let seen' = IS.insert ident seen
        children' <- mapM (\(x, y) -> (x,) <$> y) children
        let chunk = showNode ident label . foldr (\(l, Node ident' _ _) s -> showEdge ident ident' l . s) id children'
        fmap (chunk .) . go seen' . Q.pop . foldl' Q.push queue $ map snd children'
