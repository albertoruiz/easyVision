-- vim: set et ts=2 sw=2:
{-
Amortized O(1) functional queue from Chris Osaki's "Purely Functional Data
Structures".
-}
module Queue (
  Queue,
  newQ,
  emptyQ,
  enQ,
  deQ) where

data Queue a = Queue [a] [a]

instance Show a => Show (Queue a) where
  show (Queue front back) = show $ front ++ reverse back

newQ :: [a] -> Queue a
newQ xs = Queue xs []

emptyQ :: Queue a -> Bool
emptyQ (Queue [] []) = True
emptyQ _ = False

enQ :: Queue a -> a -> Queue a
enQ (Queue front back) e = Queue front (e:back)

deQ :: Queue a -> (a, Queue a)
deQ (Queue [] []) = error "deQ: empty queue"
deQ (Queue [] back) = deQ (Queue (reverse back) [])
deQ (Queue (f:front) back) = (f, Queue front back)
