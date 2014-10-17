{-# LANGUAGE TypeFamilies #-}
module Dot
    ( ID
    , Node (..)
    , Edge (..)
    , Graph (..)
    , doIfUnseen
    , showSubgraph
    ) where

import Control.Monad.ST
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.STRef
import Numeric

type ID = Int

data Node = Node String ID String

instance Show Node where
    showsPrec _ (Node prefix ident label) = showString prefix . showChar '_' . showInt ident . showString " [label=\"" . showString label . showString "\"]"

data Edge = Edge String ID ID String

instance Show Edge where
    showsPrec _ (Edge prefix from to label) = showID from . showString " -> " . showID to . showString " [label=\"" . showString label . showString "\"]"
      where
        showID ident = showString prefix . showChar '_' . showInt ident

class Graph g where
    type State g
    uniqueID :: g -> ID
    toGraph :: String -> STRef (State g) IntSet -> g -> ST (State g) ([Node], [Edge])

instance (Graph a, Graph b, State a ~ State b) => Graph (Either a b) where
    type State (Either a b) = State a
    uniqueID (Left  x) = uniqueID x
    uniqueID (Right y) = uniqueID y
    toGraph prefix seenRef (Left  x) = toGraph prefix seenRef x
    toGraph prefix seenRef (Right y) = toGraph prefix seenRef y

doIfUnseen :: STRef s IntSet -> ID -> a -> ST s a -> ST s a
doIfUnseen seenRef ident x a = do
    seen <- readSTRef seenRef
    if IS.member ident seen
        then return x
        else writeSTRef seenRef (IS.insert ident seen) >> a

showSubgraph :: Graph g => String -> g -> ST (State g) String
showSubgraph name g = do
    seen <- newSTRef IS.empty
    (nodes, edges) <- toGraph name seen g
    return . showString "subgraph {\n" . showConcat nodes . showChar '\n' . showConcat edges . showString "\n}" $ ""
  where
    showConcat :: Show a => [a] -> ShowS
    showConcat = foldr (.) id . intersperse (showChar '\n') . map shows
