module Recipe.Recipe where

import Data.List
import Data.Maybe (listToMaybe, fromJust, isJust)
import Recipe.Tree

-------------------------------------
-- RECIPE DEFINITION
-------------------------------------

data Recipe = Ingredient String
            | HeatAt Temperature Recipe
            | Wait Time Recipe
            | Combine Recipe Recipe
            | Conditional Condition Recipe
            | Transaction Recipe
            | Measure Measurement Recipe
            deriving (Show, Eq)

type Measurement = Int
type Time = Int

data Condition = CondTime Time | CondTemp Temperature | CondOpt
    deriving (Show, Eq)
-- allows pattern matching to determine type of condition
-- is there a way to do that with forall?

data Temperature = Deg Int | Low | Medium | High
    deriving (Show, Eq)

heatAt :: Temperature -> Recipe -> Recipe
heatAt = HeatAt

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

wait :: Time -> Recipe -> Recipe
wait = Wait

conditional :: Condition -> Recipe -> Recipe
conditional = Conditional

transaction :: Recipe -> Recipe
transaction = Transaction

measure :: Measurement -> Recipe -> Recipe
measure = Measure

-------------------------------------
-- RECIPE SEMANTICS
-------------------------------------

data Action =
    Input -- input and output take Recipe?
    | Output
    | Preheat Temperature
    | DoNothing Time
    | Mix Recipe Recipe
    | EvalCond Condition
    | MeasureOut Measurement Recipe
    | Hold Recipe -- tap "Holds" water
    deriving Show

-- Translate Recipe into a tree of actions
-- expand :: Recipe -> Tree Action
-- expand r@(Ingredient s)   = Node (Get s) []
-- expand r@(Heat t r')      = Node PlaceInHeat [Node (Preheat t) [], expand r']
-- expand r@(Wait t)         = Node (DoNothing t) []
-- expand r@(Combine r1 r2)  = Node Mix [expand r1, expand r2]
-- expand r@(Sequence r1 r2) = Node (DoNothing 0) [expand r1, expand r2]

-- filter topological sorts using state
-- transpose filtered list 
-- !! time with the transposed list

-- type RP = RS -> RA

-- data RS = RS
--     { rsTime :: Time
--     , rsProgress :: [Label]
--     }
--     deriving Show
    
-- type RA = [Label]
-- type TSort = [Label]

-- process :: [TSort] -> RP
-- process ts = \RS{rsTime = time, rsProgress = prog} ->
--     let opts = transpose ords
--         ords = filter (\x -> take (length prog) x == prog) ts
--     in opts `safeIx` time

-- safeIx :: [[a]] -> Int -> [a]
-- xs `safeIx` i
--     | i >= length xs = []
--     | otherwise      = xs !! i

-------------------------------------
-- CONCRETE IMPLEMENTATION
-------------------------------------

data Env = Env
    { eStations :: [Station]
    , eEntries  :: [(Recipe, StName)] -- where things start
    }

type StName = String

data Obs = ObsTemp Temperature

-- Currently assumed that all stations are
-- accessible in some way by a transfer node e.g. human
data Station = Station
    { stName     :: String
    , stInputs   :: [StName] -- List of names of stations
    , stOutputs  :: [StName] -- Better to name Connections String In|Out ?
    , stConstrF  :: ConstraintF
    , stTransfer :: Bool -- Is transfer node? could end up being Maybe f where f is how to transfer
    , stObs      :: [IO Obs]
    }

-- match a recipe against constraint function
-- returns a list of actions for the recipe if possible
type ConstraintF = Recipe -> Maybe [Action]

-- bit of an issue with CondOpt as needs to be added
-- before action i.e. after Input
addEvalCond :: Condition -> [Action] -> Maybe [Action]
addEvalCond c as = return $ init as ++ [EvalCond c, Output]

-- 2 passes:
-- 1) assign stations for recipe
-- 2) determine if transfer node needed

type Schedule = [(StName, [Action])]

-- no concurrency
-- nothing fancy
-- just find a station for each recipe
-- do things one at a time
-- schedule :: Env -> Recipe -> Maybe Schedule
-- schedule env r@(Ingredient s)  =
--     findRecipe env r >>= (\s -> [(s, [Hold r])])
-- schedule env r@(HeatAt _ _)    = schedule' env r
-- schedule env r@(Combine _ _)   = schedule' env r
-- schedule env r@(Wait _ _)      = schedule' env r
-- schedule env r@(Conditional c r') = 

-- schedule' :: Env -> Recipe -> Maybe Schedule
-- schedule' env r = assignStation env r >>=
--     (\sa -> case findRecipe env r of
--         Just s  -> [(s, [Hold r]), sa]
--         Nothing -> schedule env r ++ [sa])
        --this is an infinite loop you pillock...
        --need to call schedule env r' where
        --r' is the child recipe.

assignStation :: Env -> Recipe -> Maybe (StName, [Action])
assignStation env r = listToMaybe
    [(stName st, fromJust ma) | st <- eStations env,
                                let ma = (stConstrF st) r,
                                isJust ma]

-- Check if the recipe is stored somewhere
-- in the environment already
findRecipe :: Env -> Recipe -> Maybe StName
findRecipe env r = listToMaybe
    [snd x | x <- eEntries env, fst x == r]


-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

-- Create a list of ingredients used in a recipe
-- Doesn't yet show quantities
getIngredients :: Recipe -> [String]
getIngredients (Ingredient s)    = [s]
getIngredients (HeatAt _ r)      = getIngredients r
getIngredients (Combine r1 r2)   = getIngredients r1 ++ getIngredients r2
getIngredients (Wait _ r)        = getIngredients r
getIngredients (Conditional _ r) = getIngredients r
getIngredients (Transaction r)   = getIngredients r
getIngredients (Measure _ r)     = getIngredients r