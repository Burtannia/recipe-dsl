module Recipe.Demo where

import Recipe.Recipe
import Recipe.Tree
import Recipe.Printer

-------------------------------------
-- TEST RECIPES
-------------------------------------

-- Cup of Tea

milk, teabag, water :: Recipe
milk = Ingredient "milk"
teabag = Ingredient "teabag"
water = Ingredient "water"
sugar = Ingredient "sugar"

pintOfWater :: Recipe 
pintOfWater = measure 570 water

boilingWater, blackTea :: Recipe
boilingWater = conditional (CondTemp (Deg 100)) (heatAt (Deg 100) pintOfWater)
blackTea = wait 5 $ teabag >< boilingWater

cupOfTea :: Recipe
cupOfTea = optional $ milkTea >< sugar
    where milkTea = optional $ blackTea >< milk

-- Buttered Toast

bread, butter :: Recipe
bread = Ingredient "bread"
butter = Ingredient "butter"

toast :: Recipe
toast = conditional (CondTime 3) (heatAt (Deg 600) bread)

butteredToast :: Recipe
butteredToast = transaction $ toast >< butter

-------------------------------------
-- CUSTOM COMBINATORS
-------------------------------------

oliveOil :: Recipe
oliveOil = Ingredient "olive oil"

optional :: Recipe -> Recipe
optional = conditional CondOpt

marinate :: Recipe -> [Recipe] -> Time -> Recipe
marinate r [] t     = wait t r
marinate r [i] t    = wait t (r >< i)
marinate r (i:is) t = marinate r [foldr (><) i is] t

preheatOil :: Temperature -> Recipe -> Recipe
preheatOil t r = oil >< r
    where oil = heatAt t oliveOil

heatFor :: Temperature -> Recipe -> Time -> Recipe
heatFor temp r time = conditional (CondTime time) (heatAt temp r)

-------------------------------------
-- TEST STATIONS
-------------------------------------

env :: Env
env = Env { eStations = [kettle, workSurface, fridge, chef]
          , eEntries  = [ (Ingredient "water", "tap")
                        , (Ingredient "milk", "fridge")
                        , (Ingredient "teabag", "work surface")
                        ]
          }

tap :: Station
tap = Station {stName = "tap", stInputs = [], stOutputs = [],
    stConstrF = tapConstr, stTransfer = False, stObs = []}

tapConstr :: ConstraintF
tapConstr (Ingredient "water") = Just []
tapConstr _                    = Nothing

kettle :: Station
kettle = Station {stName = "kettle", stInputs = [], stOutputs = [],
    stConstrF = kettleConstr, stTransfer = False, stObs = [kettleTemp]}

kettleTemp :: IO Obs
kettleTemp = return $ ObsTemp $ Deg 100

kettleConstr :: ConstraintF
kettleConstr (Ingredient _)       = Just []
kettleConstr (HeatAt (Deg t) (Ingredient s))
    | t == 100 && s == "water"    = Just [Input, Output] -- is turning the kettle on part of input?
    | otherwise                   = Nothing              -- and turning off part of output?
kettleConstr (Conditional c r)    = kettleConstr r >>= addEvalCond c
kettleConstr _                    = Nothing

fridge :: Station
fridge = Station {stName = "fridge", stInputs = [], stOutputs = [],
    stConstrF = fridgeConstr, stTransfer = False, stObs = [fridgeTemp]}

fridgeTemp :: IO Obs
fridgeTemp = return $ ObsTemp $ Deg 4

fridgeConstr :: ConstraintF
fridgeConstr (Ingredient _)     = Just []
fridgeConstr (HeatAt (Deg 4) _) = Just [Input, Output]
fridgeConstr (Conditional c r)  = fridgeConstr r >>= addEvalCond c
fridgeConstr _                  = Nothing

workSurface :: Station
workSurface = Station {stName = "work surface", stInputs = [], stOutputs = [],
    stConstrF = workConstr, stTransfer = False, stObs = []}

workConstr :: ConstraintF
workConstr (Ingredient _)    = Just []
workConstr (Wait t _)        = Just [Input, DoNothing t, Output]
workConstr (Conditional c r) = workConstr r >>= addEvalCond c
workConstr _                 = Nothing

chef :: Station
chef = Station {stName = "chef", stInputs = [], stOutputs = [],
    stConstrF = chefConstr, stTransfer = True, stObs = []}

chefConstr :: ConstraintF
chefConstr (Ingredient _)    = Just []
chefConstr (Combine r1 r2)   = Just [Input, Mix r1 r2, Output]
chefConstr (Wait t _)        = Just [Input, DoNothing t, Output]
chefConstr (Conditional c r) = chefConstr r >>= addEvalCond c
chefConstr _                 = Nothing

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
ovenConstr (Conditional c r) = ovenConstr r >>= addEvalCond c
ovenConstr _                 = Nothing