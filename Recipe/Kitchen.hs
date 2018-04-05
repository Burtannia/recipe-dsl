{-# LANGUAGE RecordWildCards #-}

module Recipe.Kitchen where

import Recipe.Recipe
import Data.Tree
import Data.Monoid

data Station = Station
    { stName     :: String
    , stConstrF  :: ConstraintF
    , stObs      :: [IO Obs] -- local observables e.g. temp
    }
    
instance Show Station where
    show Station {..} = stName

type StName = String

data Obs = ObsTemp Int
         | ObsTime Time
         | ObsOpt String Bool
         deriving Show

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