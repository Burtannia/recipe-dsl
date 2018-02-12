module Recipe.Recipe where

import Prelude hiding (until)
import Data.List
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
            deriving Show

type Measurement = Int
type Time = Int

data Condition = CondTime Time | CondTemp Temperature | CondOpt
    deriving Show
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
    Input --input and output take Recipe?
    | Output
    | Preheat Temperature
    | DoNothing Time
    | Mix Recipe Recipe
    | EvalCond Condition
    | MeasureOut Measurement Recipe
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
    , eEntries  :: [(Recipe, Station)] -- where things start
    }

data Obs = ObsTemp Temperature

-- Currently assumed that all stations are
-- accessible in some way by a transfer node e.g. human
data Station = Station
    { stName     :: String
    , stInputs   :: [String] -- List of names of stations
    , stOutputs  :: [String] -- Better to name Connections String In|Out ?
    , stConstrF  :: ConstraintF
    , stTransfer :: Bool -- Is transfer node? could end up being Maybe f where f is how to transfer
    , stObs      :: [IO Obs]
    }

-- match a recipe against constraint function
-- returns a list of actions for the recipe if possible
type ConstraintF = Recipe -> Maybe [Action]

addEvalCond :: Condition -> [Action] -> Maybe [Action]
addEvalCond c as = return $ init as ++ [EvalCond c, Output]

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