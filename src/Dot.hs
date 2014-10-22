{-# LANGUAGE TypeFamilies #-}
module Dot
    ( ID
    , Node (..)
    , Graph (..)
    , doIfUnseen
    , showSubgraph
    ) where

import Control.Applicative
import Control.Monad.ST
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.STRef
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

data Node s = Node ID (ST s String) (ST s [(String, Node s)])

class Graph g where
    type State g
    toNode :: g -> Node (State g)

instance (Graph a, Graph b, State a ~ State b) => Graph (Either a b) where
    type State (Either a b) = State a
    toNode (Left  x) = toNode x
    toNode (Right y) = toNode y

doIfUnseen :: STRef s IntSet -> ID -> a -> ST s a -> ST s a
doIfUnseen seenRef ident x a = do
    seen <- readSTRef seenRef
    if IS.member ident seen
        then return x
        else writeSTRef seenRef (IS.insert ident seen) >> a

showSubgraph :: Graph g => String -> g -> ST (State g) String
showSubgraph name g = ($ "") . (showString "subgraph {\n" .) . (. showString "\n}") <$> go IS.empty (Just (toNode g, Q.empty))
  where
    go _ Nothing = return id
    go seen (Just (Node ident _ _, queue)) | IS.member ident seen = go seen $ Q.pop queue
    go seen (Just (Node ident label labelledChildren, queue)) = do
        let seen' = IS.insert ident seen
        label' <- label
        children <- labelledChildren
        let chunk = showNode name ident label' . foldr (\(l, Node ident' _ _) s -> showEdge name ident ident' l . s) id children
        fmap (chunk .) $ go seen' . Q.pop . foldl' Q.push queue . map snd $ children
