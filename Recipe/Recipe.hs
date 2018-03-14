{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Recipe.Recipe where

import Data.Tree
import Control.Monad.Trans.State
import Data.Monoid

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
            -- | Measure Measurement Recipe
            deriving (Show, Eq)

-- Stored as seconds
newtype Time = Time Int
    deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Time where
    show (Time i) = let h = i `div` 3600
                        m = (i `mod` 3600) `div` 60
                        s = (i `mod` 3600) `mod` 60 in
                    show h ++ "h "
                    ++ show m ++ "m "
                    ++ show s ++ "s"

instance Monoid Time where
    mempty = Time 0
    mappend = (+)

data Condition = CondTime Time | CondTemp Int | CondOpt
    | Condition `AND` Condition | Condition `OR` Condition
    deriving (Show, Eq)

foldCond :: (Ord a, Monoid a) => (Condition -> a) -> Condition -> a
foldCond f (c `AND` c') = (foldCond f c) `mappend` (foldCond f c')
foldCond f (c `OR` c') = max (foldCond f c) (foldCond f c')
foldCond f CondOpt = mempty
foldCond f c = f c

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

-- Nicer Conditions and Time

optional :: Recipe -> Recipe
optional = addCondition CondOpt

toTemp :: Int -> Recipe -> Recipe
toTemp t = addCondition (CondTemp t)

forTime :: Time -> Recipe -> Recipe
forTime t = addCondition (CondTime t)

hours :: Int -> Time
hours = Time . (*) 3600

minutes :: Int -> Time
minutes = Time . (*) 60

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

ingredients :: Recipe -> [String]
ingredients (Node a ts) = case a of
    GetIngredient s -> s : concatMap ingredients ts
    _ -> concatMap ingredients ts

type Label = Int

labelRecipe :: Recipe -> Tree (Label, Action)
labelRecipe r = evalState (labelRecipe' r) 1
    where
        labelRecipe' (Node a ts) = do
            ts' <- mapM labelRecipe' ts
            l <- get
            put (l + 1)
            return $ Node (l,a) ts'

labelRecipeR :: Recipe -> Tree (Label, Recipe)
labelRecipeR r = evalState (labelRecipeR' r) 1
    where
        labelRecipeR' r@(Node a ts) = do
            ts' <- mapM labelRecipeR' ts
            l <- get
            put (l + 1)
            return $ Node (l,r) ts'

-- Time to reach temp (use with CondTemp)
tempToTime :: Int -> Time
tempToTime i = Time i * 4

-- Preheat time (use with HeatAt)
-- 10m + 1m per 20 degrees
preheatTime :: Int -> Time
preheatTime i = Time t + 600
    where t = 60 * (i `div` 20)

time :: Recipe -> Time
time = foldTree (\a ts -> timeAction a + mconcat ts)

timeAction :: Action -> Time
timeAction (GetIngredient _) = 10
timeAction Heat = mempty
timeAction (HeatAt t) = preheatTime t
timeAction Wait = mempty
timeAction (Combine _) = 10
timeAction (Conditional a c) = t' + foldCond f c
    where
        t' = timeAction a
        f (CondTime t) = t
        f (CondTemp t) = tempToTime t
timeAction (Transaction a) = timeAction a
    
-- newer versions of Data.Tree implement this
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)

-- need way to evaluate chain of actions to a result
-- heat t of heat t' of r results in r being t
-- regardless of what t' was