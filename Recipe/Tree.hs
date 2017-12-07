module Recipe.Tree where

import Control.Monad.State
import Data.List (permutations)

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
    
-- Removes any node with the given label from the tree
-- Children of that node are also removed
removeNode :: Label -> Tree (Label, a) -> Tree (Label, a)
removeNode _ Empty       = Empty
removeNode l (Node a ts) = Node a [t | t <- ts', getLabel t /= l]
    where ts' = map (removeNode l) ts

-- Get the parent node of the node with the
-- given label in the given tree, Empty if no parent
getParent :: Label -> Tree (Label, a) -> Tree (Label, a)
getParent _ Empty       = Empty
getParent _ (Node _ []) = Empty
getParent l n@(Node (l', _) ts)
    | l == l'   = Empty
    | otherwise = if True `elem` (map (\t -> getLabel t == l) ts)
                    then n
                    else head $ map (getParent l) ts

-- Get label of a given node
getLabel :: Tree (Label, a) -> Label
getLabel Empty           = -1
getLabel (Node (l, _) _) = l

-- Get all topological sorts of a tree
-- NEEDS FIXING...
allKahn :: Tree (Label, a) -> [[Label]]
allKahn t = map (kahn' t []) (permutations zs)
    where zs = map getLabel $ zeroDegree t

-- Producs a list of Labels of nodes sorted topologically
kahn :: Tree (Label, a) -> [Label]
kahn t = kahn' t [] (map getLabel $ zeroDegree t)

-- ns = labels of sorted nodes
-- zs = labels of zero degree nodes
-- Presumes values stored in nodes are unique
kahn' :: Tree (Label, a) -> [Label] -> [Label] -> [Label]
kahn' _ ns []      = ns
kahn' t ns (z:zs)  = kahn' t' ns' zs'
    where
        -- take a zero degree node z and add to tail of sorted nodes
        ns' = ns ++ [z]
        -- get parent of z
        parentZ = getParent z t
        pLabel = getLabel parentZ
        -- remove z
        t' = removeNode z t
        -- if no other edges to parent then insert into sorted nodes
        zOnlyChild = length (children parentZ) <= 1
        pzNotEmpty = pLabel /= -1 
        zs' = if zOnlyChild && pzNotEmpty
                then zs ++ [pLabel]
                else zs

-- Given a label and a tree, gives the first value found
-- with the corresponding label, if not found then Nothing
findLabel :: Label -> Tree (Label, a) -> Maybe a
findLabel _ Empty             = Nothing
findLabel l (Node (l', a) ts) =
    if l == l'
        then Just a
        else case [x | x@(Just _) <- xs] of
            []     -> Nothing
            (x:xs) -> x
    where xs = map (findLabel l) ts