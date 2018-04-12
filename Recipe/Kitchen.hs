{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Data.Tree
import Data.Monoid

-- |Representation of a station in a cooking environment, for example
-- an oven.
data Station = Station
    { stName     :: StName -- ^ Name of the station, should be unique.
    , stConstrF  :: ConstraintF -- ^ Constraint function (Recipe -> Maybe Process),
                                -- given a recipe, if this station can perform
                                -- the root action of the recipe then return Just a list of processes
                                -- necessary to perform that action else return Nothing.
    , stObs      :: [IO Obs] -- ^ Local observables for example temperature of an oven.
    }
    
-- |Stations just displayed as their name.
instance Show Station where
    show Station {..} = stName

type StName = String

-- |An observable value.
data Obs = ObsTemp Int -- ^ Observable temperature.
         | ObsTime Time -- ^ Observable time.
         | ObsOpt String Bool -- ^ Observable option, labelled to correspond with a 'CondOpt'
                              -- in a recipe. True would mean to include that optional part of the recipe.
         deriving Show

-- |Given a list of observables, returns whether the given condition is true.
evalCond :: Condition -> [Obs] -> Bool
evalCond (CondTime t) os = case [o | o@(ObsTime _) <- os] of
    [] -> False
    (ObsTime t' : _) -> t == t'
evalCond (CondTemp t) os = case [o | o@(ObsTemp _) <- os] of
    [] -> False
    (ObsTemp t' : _) -> t == t'
evalCond (CondOpt s) os = case [o | o@(ObsOpt s' _) <- os, s == s'] of
    [] -> False
    (ObsOpt _ b : _) -> b
evalCond c os = getAll $ foldCond (\c -> All $ evalCond c os) c

type ConstraintF = Recipe -> Maybe [Process]

-- |Removes the 'Condition' from the root action of a recipe.
-- Does nothing if there is no condition.
popCond :: Recipe -> Recipe
popCond (Node (Conditional a c) ts) = Node a ts
popCond r = r

-- |Adds the 'EvalCond' process after the 'Input' process.
-- If 'Input' does not exist then 'EvalCond' is
-- added to the start.
addEvalCond :: Condition -> [Process] -> [Process]
addEvalCond c ps =
    if Input `elem` ps then
        addEvalCond' c ps
    else
        EvalCond c : ps
    where
        addEvalCond' c (Input:ps) = Input : EvalCond c : ps
        addEvalCond' c (p:ps) = p : addEvalCond' c ps

-- |Removes the 'Transaction' wrapper from the root action
-- of the given recipe. Does nothing if there is no
-- transaction wrapper.
popT :: Recipe -> Recipe
popT (Node (Transaction a) ts) = Node a ts
popT r = r

-- |A process that a station goes through, intended to
-- be interfaced with device specific instructions.
data Process =
    Input -- ^ Receive input.
    | Output -- ^ Output contents.
    | Preheat Int -- ^ Preheat to the given temperature.
    | DoNothing -- ^ Do nothing.
    | PCombine String -- ^ Combine contents with the given method.
    | EvalCond Condition -- ^ Evaluate the given condition.
    | MeasureOut Measurement -- ^ Measure our the given measurement of contents.
    deriving (Show, Eq)

-- |Representation of a cooking environment for example a kitchen.
data Env = Env
    { eStations :: [Station] -- ^ List of stations present in the environment.
    , eObs      :: [IO Obs] -- ^ Global observables for example time.
    }