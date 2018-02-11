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

-- TODO

-- The cooking environment is modelled as
-- a set of stations. Each station being
-- capable of performing a set of actions
-- and providing measures of any relevant
-- conditions such as temperature.

-- 2 passes:
-- 1) assign stations for recipe
-- 2) determine if transfer node needed

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
    , stOutputs  :: [String]
    , stConstrF  :: ConstraintF
    , stTransfer :: Bool -- Is transfer node?
    , stObs      :: [IO Obs]
    }

-- match a recipe against constraint function
-- returns a list of actions for the recipe if possible
type ConstraintF = Recipe -> Maybe [Action]

addEvalCond :: Condition -> [Action] -> [Action]
addEvalCond c as = init as ++ [EvalCond c, Output]

oven :: Station
oven = Station {stName = "oven", stInputs = [], stOutputs = [],
    stConstrF = ovenConstr, stTransfer = False, stObs = [ovenTemp]}

ovenTemp :: IO Obs
ovenTemp = return $ ObsTemp $ Deg 180

ovenConstr :: ConstraintF
ovenConstr (Ingredient _)    = Just []
ovenConstr (HeatAt (Deg t) _)
    | t > 120 && t < 120     = Just [Preheat (Deg t), Input, Output]
    | otherwise              = Nothing
ovenConstr (Conditional c r) = ovenConstr r >>= (\mx -> return $ addEvalCond c mx)
ovenConstr _                 = Nothing

fridge :: Station
fridge = Station {stName = "fridge", stInputs = [], stOutputs = [],
    stConstrF = fridgeConstr, stTransfer = False, stObs = [fridgeTemp]}

fridgeTemp :: IO Obs
fridgeTemp = return $ ObsTemp $ Deg 4

fridgeConstr :: ConstraintF
fridgeConstr (Ingredient _)     = Just []
fridgeConstr (HeatAt (Deg 4) _) = Just [Input, Output]
fridgeConstr (Conditional c r)  = fridgeConstr r >>= (\mx -> return $ addEvalCond c mx)
fridgeConstr _                  = Nothing

workSurface :: Station
workSurface = Station {stName = "work surface", stInputs = [], stOutputs = [],
    stConstrF = workConstr, stTransfer = False, stObs = []}

workConstr :: ConstraintF
workConstr (Ingredient _)    = Just []
workConstr (Wait t _)        = Just [Input, DoNothing t, Output]
workConstr (Conditional c r) = workConstr r >>= (\mx -> return $ addEvalCond c mx)
workConstr _                 = Nothing

chef :: Station
chef = Station {stName = "chef", stInputs = [], stOutputs = [],
    stConstrF = chefConstr, stTransfer = True, stObs = []}

chefConstr :: ConstraintF
chefConstr (Ingredient _)    = Just []
chefConstr (Combine r1 r2)   = Just [Input, Mix r1 r2, Output]
chefConstr (Wait t _)        = Just [Input, DoNothing t, Output]
chefConstr (Conditional c r) = chefConstr r >>= (\mx -> return $ addEvalCond c mx)
chefConstr _                 = Nothing

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