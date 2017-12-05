module Recipe.Tree where

import Control.Monad.State

data Tree a = Empty | Node a [Tree a]
    deriving Show

testT :: Tree Int
testT = Node 1 [Node 2 [Node 3 []], Node 4 []]

instance Functor Tree where
    -- fmap :: (a -> b) -> fa -> fb
    fmap _ Empty       = Empty
    fmap f (Node a ts) = Node (f a) (map (fmap f) ts)

type Label = Int

-- Label tree from 0 breadth-first 
labelTree :: Tree a -> Tree (Label, a)
labelTree Empty = Empty
labelTree t = evalState (labelTree' t) 0

labelTree' :: Tree a -> State Label (Tree (Label, a))
labelTree' Empty       = return Empty
labelTree' (Node a ts) = do
    l <- get
    put (l + 1)
    ts' <- mapM labelTree' ts
    return $ Node (l, a) ts'
    
-- Returns children of a node
children :: Tree a -> [Tree a]
children Empty       = []
children (Node _ ts) = ts

-- True if node has no children
leaf :: Tree a -> Bool
leaf (Node _ (t:ts)) = False
leaf _               = True

-- Calculate the degree of a node in the tree
calcDegree :: Tree a -> Int
calcDegree Empty       = 0
calcDegree (Node _ ts) = length ts

-- Produce a list of nodes with a degree of zero
zeroDegree :: Tree a -> [Tree a]
zeroDegree n =
    if calcDegree n == 0
        then (n:ns)
        else ns
    where ns = concatMap zeroDegree (children n)