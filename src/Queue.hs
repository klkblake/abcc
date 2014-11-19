module Queue
    ( Queue
    , empty
    , push
    , pop
    ) where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

push :: Queue a -> a -> Queue a
push (Queue f b) x = Queue f $ x:b

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) b) = Just (x, Queue f b)
pop (Queue [] b@(_:_)) = pop $ Queue (reverse b) []
pop (Queue [] []) = Nothing
