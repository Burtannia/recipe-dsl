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

time :: Recipe -> Time
time = foldTree (\a ts -> time' a + mconcat ts)
    where
        time' :: Action -> Time
        time' (GetIngredient _) = 10
        time' Heat = mempty
        time' (HeatAt t) = Time t' + 600
            where t' = 60 * (t `div` 20)
        time' Wait = mempty
        time' (Combine _) = 10
        time' (Conditional a c) = t' + foldCond f c
            where
                t' = time' a
                f (CondTime t) = t
                f (CondTemp t) = Time t * 4
        time' (Transaction a) = time' a
    
-- newer versions of Data.Tree implement this
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)

-- need way to evaluate chain of actions to a result
-- heat t of heat t' of r results in r being t
-- regardless of what t' was


-- -- bit of an issue with CondOpt as needs to be added
-- -- before action i.e. after Input
-- addEvalCond :: Condition -> [Action] -> Maybe [Action]
-- addEvalCond c as = return $ init as ++ [EvalCond c, Output]

-- type Schedule = [(StName, [Action])]

-- entryPoint :: Env -> Recipe -> Maybe StName
-- entryPoint env r = listToMaybe $ findRecipe env r

-- -- Check if the recipe is stored somewhere
-- -- in the environment already
-- findRecipe :: Env -> Recipe -> [StName]
-- findRecipe env r = [snd x | x <- eEntries env,
--                             fst x == r]

-- makeSchedule :: Env -> Recipe -> Maybe Schedule
-- makeSchedule env r = case r of
--     (Ingredient _)     -> makeSchedule' (const Nothing)
--     (HeatAt _ r')      -> makeSchedule' (singleChild r')
--     (Combine r1 r2)    -> makeSchedule' (doubleChild r1 r2)
--     (Wait _ r')        -> makeSchedule' (singleChild r')
--     (Conditional _ r') -> makeSchedule' (singleChild r')
--     (Transaction r')   -> makeSchedule' (singleChild r') -- does this matter atm as we are doing things in order anyway?
--     (Measure _ r')     -> makeSchedule' (singleChild r')
--     where
--         makeSchedule' scheduleCons = case entryPoint env r of
--             Nothing -> assignStation env r >>= scheduleCons
--             Just s  -> Just [(s, [Hold r])]
--         singleChild r' sa = (++) <$> makeSchedule env r' <*> Just [sa]
--         doubleChild r1 r2 sa = (++) <$> ((++)
--             <$> makeSchedule env r1 <*>
--             makeSchedule env r2) <*> Just [sa]

-- assignStation :: Env -> Recipe -> Maybe (StName, [Action])
-- assignStation env r = listToMaybe
--     [(stName st, fromJust ma) | st <- eStations env,
--                                 let ma = (stConstrF st) r,
--                                 isJust ma]