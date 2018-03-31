{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Data.Tree

data Station = Station
    { stName     :: String
    , stInputs   :: [StName]
    , stOutputs  :: [StName]
    , stConstrF  :: ConstraintF
    , stObs      :: [IO Obs] -- local observables e.g. temp
    }
    
instance Show Station where
    show Station {..} = stName

type StName = String

data Obs = ObsTemp Int
         | ObsTime Time
         deriving Show

type ConstraintF = Recipe -> Maybe [Process]

popCond :: Recipe -> Recipe
popCond (Node (Conditional a c) ts) = Node a ts
popCond r = r

addEvalCond :: Condition -> [Process] -> [Process]
addEvalCond c (Input:ps) = Input : EvalCond c : ps
addEvalCond c ps = EvalCond c : ps

popT :: Recipe -> Recipe
popT (Node (Transaction a) ts) = Node a ts
popT r = r

data Process =
    Input
    | Output
    | Preheat Int
    | DoNothing
    | PCombine String
    | EvalCond Condition
    | MeasureOut Measurement
    deriving Show

data Env = Env
    { eStations :: [Station]
    , eObs      :: [IO Obs] -- global observables e.g. time
    }