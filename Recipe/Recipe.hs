{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Recipe.Recipe where

import           Control.Monad.Trans.State
import           Data.Tree hiding (foldTree)
import QuickSpec
import Test.QuickCheck
import Data.List (sort)

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

type Recipe = Tree Action

data Action = GetIngredient String
            | Heat
            | HeatAt Int
            | Wait
            | Combine String
            | Conditional Action Condition
            | Transaction Action
            | Measure Measurement
            deriving (Show, Eq, Ord)

-- |Ordering compares the number of different topological
-- sorts of each recipe.
instance Ord Recipe where
    compare t1 t2 = let xs = sort $ topologicals t1
                        ys = sort $ topologicals t2
                     in compare xs ys

-- |Two Recipes are equal if their sets of topological sorts
-- are equal.
instance {-# OVERLAPPING #-} Eq Recipe where
    (==) r1 r2 = compare r1 r2 == EQ

-- |Time is a wrapper around an Int representing seconds.
newtype Time = Time Int
    deriving (Eq, Ord, Num, Real, Enum, Integral)

-- |Time is printed in hms format.
instance Show Time where
    show (Time i) = let h = i `div` 3600
                        m = (i `mod` 3600) `div` 60
                        s = (i `mod` 3600) `mod` 60 in
                    show h ++ "h "
                    ++ show m ++ "m "
                    ++ show s ++ "s"

instance Monoid Time where
    mempty = 0
    mappend = (+)

data Condition = CondTime Time | CondTemp Int | CondOpt String
    | Condition `AND` Condition | Condition `OR` Condition
    deriving (Show, Eq, Ord)

foldCond :: (Ord a, Monoid a) => (Condition -> a) -> Condition -> a
foldCond f (c `AND` c') = (foldCond f c) `mappend` (foldCond f c')
foldCond f (c `OR` c')  = max (foldCond f c) (foldCond f c')
foldCond f c            = f c

data Measurement = Count Int | Grams Int | Milliletres Int
    deriving (Eq)

getMeasure :: Measurement -> Int
getMeasure (Count i) = i
getMeasure (Grams i) = i
getMeasure (Milliletres i) = i

instance Ord Measurement where
    compare a b = compare (getMeasure a) (getMeasure b)

instance Show Measurement where
    show (Count i) = show i
    show (Grams i) = show i ++ "g"
    show (Milliletres i) = show i ++ "ml"

ingredient :: String -> Recipe
ingredient s = Node (GetIngredient s) []

heat :: Recipe -> Recipe
heat r = Node Heat [r]

heatAt :: Int -> Recipe -> Recipe
heatAt temp r = Node (HeatAt temp) [r]

wait :: Recipe -> Recipe
wait r = Node Wait [r]

combine :: String -> Recipe -> Recipe -> Recipe
combine s r1 r2 = Node (Combine s) [r1, r2]

addCondition :: Condition -> Recipe -> Recipe
addCondition c (Node a ts) = case a of
    Conditional a' c' -> Node a'' ts
        where a'' = Conditional a' (c .&& c')
    _ -> Node (Conditional a c) ts

(.&&) :: Condition -> Condition -> Condition
(.&&) = AND

(.||) :: Condition -> Condition -> Condition
(.||) = OR

transaction :: Recipe -> Recipe
transaction (Node a ts) = Node (Transaction a) ts

measure :: Measurement -> Recipe -> Recipe
measure m r = Node (Measure m) [r]

-- Nicer Conditions and Time

optional :: String -> Recipe -> Recipe
optional s = addCondition (CondOpt s)

toTemp :: Int -> Recipe -> Recipe
toTemp t = addCondition (CondTemp t)

forTime :: Time -> Recipe -> Recipe
forTime t = addCondition (CondTime t)

hours :: Time -> Time
hours = (*) 3600

minutes :: Time -> Time
minutes = (*) 60

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

foldRecipe :: Monoid a => (Action -> a) -> Recipe -> a
foldRecipe f (Node a ts) =
    let vs = map (foldRecipe f) ts
     in f a `mappend` (mconcat vs)

ingredients :: Recipe -> [String]
ingredients = foldRecipe f
    where
        f (GetIngredient s) = [s]
        f _ = []

ingredientsQ :: Recipe -> [(String, Measurement)]
ingredientsQ (Node (Measure m) ts) = case ts of
    [Node (GetIngredient s) _] -> [(s,m)]
    _ -> concatMap ingredientsQ ts
ingredientsQ (Node (GetIngredient s) _) = [(s, Count 0)]
ingredientsQ (Node _ ts) = concatMap ingredientsQ ts

type Label = Int

labelRecipe :: Recipe -> Tree Label
labelRecipe r = fmap fst (labelRecipeR r)

labelRecipeR :: Recipe -> Tree (Label, Recipe)
labelRecipeR r = evalState (labelRecipeR' r) 1
    where
        labelRecipeR' r@(Node a ts) = do
            ts' <- mapM labelRecipeR' ts
            l <- get
            put (l + 1)
            return $ Node (l,r) ts'

labelRecipeA :: Recipe -> Tree (Label, Action)
labelRecipeA r = fmap (\(l,r) -> (l, rootLabel r))
    (labelRecipeR r)

-- Time to reach temp (use with CondTemp)
tempToTime :: Int -> Time
tempToTime i = Time i * 2

-- Preheat time (use with HeatAt)
-- 10m
preheatTime :: Int -> Time
preheatTime = const $ Time 600

time :: Recipe -> Time
time = foldRecipe timeAction

timeAction :: Action -> Time
timeAction (GetIngredient _) = 10
timeAction Heat = 0
timeAction (HeatAt t) = preheatTime t
timeAction Wait = 0
timeAction (Combine _) = 10
timeAction (Conditional a c) = t' + foldCond f c
    where
        t' = timeAction a
        f (CondTime t) = t
        f (CondTemp t) = tempToTime t
        f (CondOpt s) = 0
timeAction (Transaction a) = timeAction a
timeAction (Measure m) = 10

topologicals :: Recipe -> [[Action]]
topologicals (Node a []) = [[a]]
topologicals t = concat
    [map (a:) (topologicals' l) | l@(Node a _) <- ls]
    where
        topologicals' l = topologicals $ removeFrom t l
        ls = leaves t

isLeaf :: Recipe -> Bool
isLeaf (Node _ []) = True
isLeaf _           = False

leaves :: Recipe -> [Recipe]
leaves (Node a []) = [Node a []]
leaves (Node a ts) = concatMap leaves ts

-- Removes all occurences of a sub tree from the given tree.
-- Removing a tree from itself does nothing.
removeFrom :: Recipe -> Recipe -> Recipe
removeFrom t@(Node a ts) toRem = Node a ts''
    where
        ts'  = deleteAll toRem ts
        ts'' = map (\t -> removeFrom t toRem) ts'

deleteAll :: Recipe -> [Recipe] -> [Recipe]
deleteAll _ [] = []
deleteAll x (y:ys)
    | x == y = deleteAll x ys
    | otherwise = y : deleteAll x ys