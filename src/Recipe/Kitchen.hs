{-# LANGUAGE RecordWildCards #-}

{-|
This module contains the definitions
for the modelling of a cooking environment.
There are also various utility functions.
-}

module Recipe.Kitchen where

import           Data.Monoid
import           Data.Tree
import           Recipe.Recipe

-- |Representation of a station in a cooking environment, for example
-- an oven.
data Station = Station
    { stName    :: StName -- ^ Name of the station, should be unique.
    , stConstrF :: ConstraintF -- ^ Constraint function (Recipe -> Maybe Process),
                                -- given a recipe, if this station can perform
                                -- the root action of the recipe then return Just a list of processes
                                -- necessary to perform that action else return Nothing.
    , stObs     :: [IO Obs] -- ^ Local observables for example temperature of an oven.
    }

-- |Stations just displayed as their name.
instance Show Station where
    show Station {..} = stName

type StName = String

-- |An observable value.
data Obs = ObsTemp Int -- ^ Observable temperature.
         | ObsTime Time -- ^ Observable time.
         deriving (Show, Eq, Ord)

-- |A decision for a `CondOpt`.
data Option = Option String Bool

-- |Given a list of `Option`s, find whether an `Option` with the
-- given name is true or false.
evalOpt :: String -> [Option] -> Bool
evalOpt s os = case [b | Option s' b <- os, s == s'] of
    []     -> False
    (b:bs) -> b

-- |Given a list of observables, returns whether the given condition is true.
evalCond :: Condition -> [Obs] -> Bool
evalCond (CondTime t) os = case [o | o@(ObsTime _) <- os] of
    []  -> False
    os' -> True `elem` (map (\(ObsTime t') -> t' >= t) os')
evalCond (CondTemp t) os = case [o | o@(ObsTemp _) <- os] of
    []  -> False
    os' -> True `elem` (map (\(ObsTemp t') -> t == t') os')
evalCond c os = getAll $ foldCond (\c -> All $ evalCond c os) c

-- |Takes a condition of time and the observable representing
-- the global time at which the action wrapped by the condition was started
-- and adjusts the condition to absolute time rather than relative time.
-- Does nothing if the condition is not CondTIme or the observable
-- is not ObsTime.
adjustTime :: Condition -> Obs -> Condition
adjustTime (CondTime t) (ObsTime t') = CondTime (t + t')
adjustTime (AND c1 c2) t             = AND (adjustTime c1 t) (adjustTime c2 t)
adjustTime (OR c1 c2) t              = OR (adjustTime c1 t) (adjustTime c2 t)
adjustTime c _                       = c

type ConstraintF = Recipe -> Maybe [Process]

-- |Removes the 'Condition' from the root action of a recipe.
-- Does nothing if there is no condition.
popCond :: Recipe -> Recipe
popCond (Node (Conditional a c) ts) = Node a ts
popCond r                           = r

-- |Adds the 'EvalCond' process after the 'Input' and 'Preheat' processes.
-- If 'Input' does not exist then 'EvalCond' is
-- added to the start.
addEvalCond :: Condition -> [Process] -> [Process]
addEvalCond c ps =
    if P_Input `elem` ps then
        addEvalCond' c ps
    else
        P_EvalCond c : ps
    where
        addEvalCond' c (P_Input : P_Preheat t : ps) = P_Input : P_Preheat t : P_EvalCond c : ps
        addEvalCond' c (P_Input : ps) = P_Input : P_EvalCond c : ps
        addEvalCond' c (p:ps) = p : addEvalCond' c ps

-- |Removes the 'Transaction' wrapper from the root action
-- of the given recipe. Does nothing if there is no
-- transaction wrapper.
popT :: Recipe -> Recipe
popT (Node (Transaction a) ts) = Node a ts
popT r                         = r

-- |A process that a station goes through, intended to
-- be interfaced with device specific instructions.
data Process =
    P_Input -- ^ Receive input.
    | P_Output -- ^ Output contents.
    | P_GetIngredient String -- ^ Get an ingredient.
    | P_Preheat Int -- ^ Preheat to the given temperature.
    | P_Combine String -- ^ Combine contents with the given method.
    | P_EvalCond Condition -- ^ Evaluate the given condition.
    | P_Measure Measurement -- ^ Measure the given measurement of contents.
    deriving (Show, Eq)

-- |Representation of a cooking environment for example a kitchen.
data Env = Env
    { eStations :: [Station] -- ^ List of stations present in the environment.
    , eObs      :: [IO Obs] -- ^ Global observables for example time.
    , eOpts     :: [Option] -- ^ List of decisions for any optional steps.
    }

------------------------
-- Utility Functions
------------------------

-- |Increments all the 'EvalCond's that contain
-- a condition of time 'CondTime' in a list of processes.
incTimeConds :: [Process] -> [Process]
incTimeConds [] = []
incTimeConds (p@(P_EvalCond c) : ps) =
    P_EvalCond (adjustTime c (ObsTime 1)) : (incTimeConds ps)
incTimeConds (p:ps) = p : incTimeConds ps

-- |Applied 'adjustTime' to all 'EvalCond' in a list of
-- processes. Passed global time observable.
condsToAbsolute :: [Process] -> Obs -> [Process]
condsToAbsolute [] _ = []
condsToAbsolute (p@(P_EvalCond c) : ps) o =
    P_EvalCond (adjustTime c o) : (condsToAbsolute ps o)
condsToAbsolute (p:ps) o = p : condsToAbsolute ps o

-- |Extracts all temperatures from 'CondTemp's found within a condition.
extractTemps :: Condition -> [Int]
extractTemps (CondTemp t) = [t]
extractTemps (AND c1 c2)  = extractTemps c1 ++ extractTemps c2
extractTemps (OR c1 c2)   = extractTemps c1 ++ extractTemps c2
extractTemps _            = []

-- |Extracts all 'CondTemp's found within a condition.
extractTempConds :: Condition -> [Condition]
extractTempConds c@(CondTemp _) = [c]
extractTempConds (AND c1 c2) = extractTempConds c1 ++ extractTempConds c2
extractTempConds (OR c1 c2) = extractTempConds c1 ++ extractTempConds c2
extractTempConds _ = []

-- |If a condition contains temperatures, checks that they
-- meet the constraint passed e.g. > 50.
valTemp :: Condition -> (Int -> Bool) -> Bool
valTemp c f = all (== True) $ map f (extractTemps c)

-- |Gets the temperature from a list of observables.
-- Throws error if there are no 'ObsTemp's.
getTemp :: [Obs] -> Int
getTemp os = let ts = [t | ObsTemp t <- os]
              in if ts == [] then
                    error "No Observable Temperature"
                 else
                    head ts
