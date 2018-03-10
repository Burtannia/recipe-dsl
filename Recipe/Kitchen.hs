{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe

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

-- might want to consider an intermmediate translation
-- like assembler, to handle Heat you do these things
-- then each station has its way of handling those things
data Process =
    Input
    | Output
    | Preheat Int
    | DoNothing
    | PCombine String
    | EvalCond Condition
    -- | MeasureOut Measurement Recipe
    deriving Show

data Env = Env
    { eStations :: [Station]
    , eObs      :: [IO Obs] -- global observables e.g. time
    }