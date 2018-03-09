module Recipe.Recipe where

import Data.Tree
import Control.Monad.State

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

type Recipe = Tree Action

data Action = GetIngredient String
            | HeatAt Int
            | Wait
            | Combine String
            | Conditional Action Condition
            | Transaction Action
            -- | Measure Measurement Recipe
            deriving Show

data Condition = CondTemp Int | CondTime Int | CondOpt
    deriving Show

ingredient :: String -> Recipe
ingredient s = Node (GetIngredient s) []

heatAt :: Int -> Recipe -> Recipe
heatAt temp r = Node (HeatAt temp) [r]

wait :: Recipe -> Recipe
wait r = Node Wait [r]

combine :: String -> Recipe -> Recipe -> Recipe
combine s r1 r2 = Node (Combine s) [r1, r2]

addCondition :: Condition -> Recipe -> Recipe
addCondition cond (Node a ts) = Node (Conditional a cond) ts

transaction :: Recipe -> Recipe
transaction (Node a ts) = Node (Transaction a) ts

-- Nicer Conditions

optional :: Recipe -> Recipe
optional = addCondition CondOpt

toTemp :: Int -> Recipe -> Recipe
toTemp t = addCondition (CondTemp t)

forTime :: Int -> Recipe -> Recipe
forTime t = addCondition (CondTime t)

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

type Label = Int

labelRecipe :: Recipe -> Tree (Label, Action)
labelRecipe r = evalState (labelRecipe' r) 1
    where
        labelRecipe' (Node a ts) = do
            ts' <- mapM labelRecipe' ts
            l <- get
            put (l + 1)
            return $ Node (l,a) ts'

-- -------------------------------------
-- -- RECIPE SEMANTICS
-- -------------------------------------

-- data Action =
--     Input -- input and output take Recipe?
--     | Output
--     | Preheat Temperature
--     | DoNothing Time
--     | Mix Recipe Recipe
--     | EvalCond Condition
--     | MeasureOut Measurement Recipe
--     | Hold Recipe -- tap "Holds" water
--     deriving Show

-- -------------------------------------
-- -- CONCRETE IMPLEMENTATION
-- -------------------------------------

-- data Env = Env
--     { eStations :: [Station]
--     , eEntries  :: [(Recipe, StName)] -- where things start
--     }

-- type StName = String

-- data Obs = ObsTemp Temperature

-- -- Currently assumed that all stations are
-- -- accessible in some way by a transfer node e.g. human
-- data Station = Station
--     { stName     :: String
--     , stInputs   :: [StName] -- List of names of stations
--     , stOutputs  :: [StName] -- Better to name Connections String In|Out ?
--     , stConstrF  :: ConstraintF
--     , stTransfer :: Bool -- Is transfer node? could end up being Maybe f where f is how to transfer
--     , stObs      :: [IO Obs]
--     }

-- -- match a recipe against constraint function
-- -- returns a list of actions for the recipe if possible
-- type ConstraintF = Recipe -> Maybe [Action]

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