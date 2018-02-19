module Recipe.Recipe where

import Data.List
import Data.Maybe (listToMaybe, fromJust, isJust)
import Recipe.Tree
import Control.Applicative
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap

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
transaction r@(Ingredient _) = r
transaction r = Transaction r

measure :: Measurement -> Recipe -> Recipe
measure = Measure

-------------------------------------
-- RECIPE LABELLING
-------------------------------------

createTable :: Recipe -> HashMap Int Recipe
createTable r = evalState (createTable' r) 0
    where
        createTable' r = case r of
            Ingredient _ -> do
                k <- incKey
                return $ HMap.singleton k r
            HeatAt _ r' -> singleChild r r'
            Wait _ r' -> singleChild r r'
            Combine r1 r2 -> doubleChild r r1 r2
            Conditional _ r' -> singleChild r r'
            Transaction r' -> singleChild r r'
            Measure _ r' -> singleChild r r'
            where
                incKey = do
                    k <- get
                    put (k + 1)
                    return k
                singleChild par ch = do
                    k <- incKey
                    let hmapP = HMap.singleton k par
                    hmapC <- createTable' ch
                    return $ HMap.union hmapP hmapC
                doubleChild par ch1 ch2 = do
                    k <- incKey
                    let hmapP = HMap.singleton k par
                    hmapC1 <- createTable' ch1
                    hmapC2 <- createTable' ch2
                    return $ HMap.union hmapP
                        (HMap.union hmapC1 hmapC2)

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

type Schedule = [(StName, [Action])]

entryPoint :: Env -> Recipe -> Maybe StName
entryPoint env r = listToMaybe $ findRecipe env r

-- Check if the recipe is stored somewhere
-- in the environment already
findRecipe :: Env -> Recipe -> [StName]
findRecipe env r = [snd x | x <- eEntries env,
                            fst x == r]

makeSchedule :: Env -> Recipe -> Maybe Schedule
makeSchedule env r = case r of
    (Ingredient _)     -> makeSchedule' (const Nothing)
    (HeatAt _ r')      -> makeSchedule' (singleChild r')
    (Combine r1 r2)    -> makeSchedule' (doubleChild r1 r2)
    (Wait _ r')        -> makeSchedule' (singleChild r')
    (Conditional _ r') -> makeSchedule' (singleChild r')
    (Transaction r')   -> makeSchedule' (singleChild r') -- does this matter atm as we are doing things in order anyway?
    (Measure _ r')     -> makeSchedule' (singleChild r')
    where
        makeSchedule' scheduleCons = case entryPoint env r of
            Nothing -> assignStation env r >>= scheduleCons
            Just s  -> Just [(s, [Hold r])]
        singleChild r' sa = (++) <$> makeSchedule env r' <*> Just [sa]
        doubleChild r1 r2 sa = (++) <$> ((++)
            <$> makeSchedule env r1 <*>
            makeSchedule env r2) <*> Just [sa]

assignStation :: Env -> Recipe -> Maybe (StName, [Action])
assignStation env r = listToMaybe
    [(stName st, fromJust ma) | st <- eStations env,
                                let ma = (stConstrF st) r,
                                isJust ma]

-- 1) determine entry point for a recipe
-- 2) assign stations for recipe
-- 3) determine if transfer node needed

-- schedule :: Env -> Recipe -> Maybe Schedule
-- schedule env r = 
--     where
--         s = makeSchedule env r

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