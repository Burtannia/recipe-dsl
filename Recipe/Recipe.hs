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
    Get String
    -- Heat
    | Preheat Temperature
    | Refrigerate
    | PlaceInHeat
    | LeaveRoomTemp
    | Freeze
    -- Wait
    | DoNothing Time
    -- Combine
    | PlaceAbove
    | PlaceIn
    | PourOver
    | Mix
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

type Env = [Station]

data Obs = ObsTemp Temperature

data Station = Station
    { stName    :: String
    , stMethods :: [(Recipe, Method)]
    , stObs     :: [IO Obs]
    }

type Method = [Action]

oven :: Station
oven = Station {stName = "oven", stMethods = [], stObs = [ovenTemp]}

ovenTemp :: IO Obs
ovenTemp = return $ ObsTemp $ Deg 180

fridge :: Station
fridge = Station {stName = "fridge", stMethods = [], stObs = []}
-- need some way to model strict ranges of temperatures
-- fridge "heats" but only to 4 degrees or whatever the temp is.

chef :: Station
chef = Station {stName = "chef", stMethods = [], stObs = []}

data Capability = Heat Int Int | Transfer

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