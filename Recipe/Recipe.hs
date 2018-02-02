{-# LANGUAGE ExistentialQuantification #-}

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
            deriving Show

type Quantity = Int
type Time = Int

data Condition = CondTime Time | CondTemp Temperature
    deriving Show
-- allows pattern matching to determine type of condition
-- is there a way to do that with forall?

-- Void is used for devices that do not specify a
-- temperature e.g. a kettle
data Temperature = Deg Int | Low | Medium | High | Void
    deriving (Show, Eq)

heatAt :: Temperature -> Recipe -> Recipe
heatAt = HeatAt

(><) :: Recipe -> Recipe -> Recipe
(><) = Combine

wait :: Time -> Recipe -> Recipe
wait = Wait

cond :: Condition -> Recipe -> Recipe
cond = Conditional

-- With adding the Measure combinator we may be able to extract
-- any quantifiable combinators into the measure combinator
-- e.g. Wait is a quantity of time, heat int recipe is a quantity of heat

-- measure :: Quantity -> Recipe -> Recipe
-- measure = Measure

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
    { stName :: String
    , stActs :: [Action]
    , stObs  :: [IO Obs]
    }

oven :: Station
oven = Station {stName = "oven", stActs = [], stObs = [ovenTemp]}
-- Some Actions e.g. Preheat take an argument which could be an issue...

ovenTemp :: IO Obs
ovenTemp = return $ ObsTemp $ Deg 180

fridge :: Station
fridge = Station {stName = "fridge", stActs = [], stObs = []}
-- need some way to model strict ranges of temperatures
-- fridge "heats" but only to 4 degrees or whatever the temp is.

-------------------------------------
-- UTILITY FUNCTIONS
-------------------------------------

-- Create a list of ingredients used in a recipe
getIngredients :: Recipe -> [String]
getIngredients (Ingredient s)    = [s]
getIngredients (HeatAt _ r)      = getIngredients r
getIngredients (Combine r1 r2)   = getIngredients r1 ++ getIngredients r2
getIngredients (Wait _ r)        = getIngredients r
getIngredients (Conditional _ r) = getIngredients r