module Recipe.Tree where

import Control.Monad.State

data Tree a = Node a [Tree a]
    deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> fa -> fb
    fmap f (Node a ts) = Node (f a) (map (fmap f) ts)

type Label = Int

data LTree a = LNode Label a [LTree a]
    deriving Show

labelTree :: Tree a -> LTree a
labelTree t = evalState (labelTree' t) 0

labelTree' :: Tree a -> State Label (LTree a)
labelTree' (Node a ts) = do
    l <- get
    put (l + 1)
    ts' <- mapM labelTree' ts
    return $ LNode l a ts'

testT :: Tree Int
testT = Node 1 [Node 2 [Node 3 []], Node 4 []]