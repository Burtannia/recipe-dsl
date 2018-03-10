module Recipe.Demo where

import Recipe.Recipe
import Recipe.Printer
--import Recipe.QS

-------------------------------------
-- TEST RECIPES
-------------------------------------

-- Cup of Tea

water, teabag, milk :: Recipe
water = ingredient "water"
teabag = ingredient "teabag"
milk = ingredient "milk"

cupOfTea :: Recipe
cupOfTea = optional $ combine "mix" milk blackTea
    where
        boilingWater = addCondition (CondTemp 100) (heat water)
        wait5 = \r -> addCondition (CondTime 300) (wait r)
        blackTea = wait5 $ combine "pour" boilingWater teabag

cupOfTea' :: Recipe
cupOfTea' = optional
    $ combine "mix" milk
    $ waitFor (minutes 5)
    $ combine "pour" (heatTo 100 water) teabag

-- Buttered Toast

butter, bread :: Recipe
butter = ingredient "butter"
bread = ingredient "bread"

butteredToast :: Recipe
butteredToast = transaction $ combine "spread" butter toast
    where toast = addCondition (CondTime 3) (heatAt 600 bread)

-------------------------------------
-- CUSTOM COMBINATORS
-------------------------------------

waitFor :: Time -> Recipe -> Recipe
waitFor t r = forTime t (wait r)

multiCombine :: String -> Recipe -> [Recipe] -> Recipe
multiCombine s r [] = r
multiCombine s r rs = foldr (combine s) r rs

marinate :: Recipe -> [Recipe] -> Time -> Recipe
marinate r [] t     = waitFor t r
marinate r [i] t    = waitFor t (combine "place in" r i)
marinate r (i:is) t = waitFor t (combine "place in" r r')
    where r' = multiCombine "mix" i is

heatFor :: Time -> Recipe -> Recipe
heatFor t = forTime t . heat

heatTo :: Int -> Recipe -> Recipe
heatTo t = toTemp t . heat

heatAtFor :: Int -> Time -> Recipe -> Recipe
heatAtFor temp time = forTime time . heatAt temp

heatAtTo :: Int -> Int -> Recipe -> Recipe
heatAtTo atTmp toTmp = toTemp toTmp . heatAt atTmp

oliveOil :: Recipe
oliveOil = ingredient "olive oil"

preheatOil :: Int -> Int -> Recipe
preheatOil = \atTmp toTmp ->
    heatAtTo atTmp toTmp oliveOil

-------------------------------------
-- TEST STATIONS
-------------------------------------

-- env :: Env
-- env = Env { eStations = [kettle, workSurface, fridge, chef]
--           , eEntries  = [ (Ingredient "water", "tap")
--                         , (Ingredient "milk", "fridge")
--                         , (Ingredient "teabag", "work surface")
--                         ]
--           }

-- tap :: Station
-- tap = Station {stName = "tap", stInputs = [], stOutputs = [],
--     stConstrF = tapConstr, stTransfer = False, stObs = []}

-- tapConstr :: ConstraintF
-- tapConstr (Ingredient "water") = Just []
-- tapConstr _                    = Nothing

-- kettle :: Station
-- kettle = Station {stName = "kettle", stInputs = [], stOutputs = [],
--     stConstrF = kettleConstr, stTransfer = False, stObs = [kettleTemp]}

-- kettleTemp :: IO Obs
-- kettleTemp = return $ ObsTemp $ Deg 100

-- kettleConstr :: ConstraintF
-- kettleConstr (Ingredient _)       = Just []
-- kettleConstr (HeatAt (Deg t) (Ingredient s))
--     | t == 100 && s == "water"    = Just [Input, Output] -- is turning the kettle on part of input?
--     | otherwise                   = Nothing              -- and turning off part of output?
-- kettleConstr (Conditional c r)    = kettleConstr r >>= addEvalCond c
-- kettleConstr _                    = Nothing

-- fridge :: Station
-- fridge = Station {stName = "fridge", stInputs = [], stOutputs = [],
--     stConstrF = fridgeConstr, stTransfer = False, stObs = [fridgeTemp]}

-- fridgeTemp :: IO Obs
-- fridgeTemp = return $ ObsTemp $ Deg 4

-- fridgeConstr :: ConstraintF
-- fridgeConstr (Ingredient _)     = Just []
-- fridgeConstr (HeatAt (Deg 4) _) = Just [Input, Output]
-- fridgeConstr (Conditional c r)  = fridgeConstr r >>= addEvalCond c
-- fridgeConstr _                  = Nothing

-- workSurface :: Station
-- workSurface = Station {stName = "work surface", stInputs = [], stOutputs = [],
--     stConstrF = workConstr, stTransfer = False, stObs = []}

-- workConstr :: ConstraintF
-- workConstr (Ingredient _)    = Just []
-- workConstr (Wait t _)        = Just [Input, DoNothing t, Output]
-- workConstr (Conditional c r) = workConstr r >>= addEvalCond c
-- workConstr _                 = Nothing

-- chef :: Station
-- chef = Station {stName = "chef", stInputs = [], stOutputs = [],
--     stConstrF = chefConstr, stTransfer = True, stObs = []}

-- chefConstr :: ConstraintF
-- chefConstr (Ingredient _)    = Just []
-- chefConstr (Combine r1 r2)   = Just [Input, Mix r1 r2, Output]
-- chefConstr (Wait t _)        = Just [Input, DoNothing t, Output]
-- chefConstr (Conditional c r) = chefConstr r >>= addEvalCond c
-- chefConstr _                 = Nothing

-- oven :: Station
-- oven = Station {stName = "oven", stInputs = [], stOutputs = [],
--     stConstrF = ovenConstr, stTransfer = False, stObs = [ovenTemp]}

-- ovenTemp :: IO Obs
-- ovenTemp = return $ ObsTemp $ Deg 180

-- ovenConstr :: ConstraintF
-- ovenConstr (Ingredient _)    = Just []
-- ovenConstr (HeatAt (Deg t) _)
--     | t > 120 && t < 240     = Just [Preheat (Deg t), Input, Output]
--     | otherwise              = Nothing
-- ovenConstr (Conditional c r) = ovenConstr r >>= addEvalCond c
-- ovenConstr _                 = Nothing