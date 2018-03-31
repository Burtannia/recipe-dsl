module Recipe.Demo where

import Recipe.Recipe
import Recipe.Printer
import Recipe.Kitchen
import Recipe.Scheduler
import Data.Tree
import Recipe.Properties
--import Recipe.QS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
    $ combine "mix" (measure (Milliletres 10) milk)
        $ waitFor (minutes 5)
        $ combine "pour" (heatTo 100
            $ measure (Milliletres 300) water)
            $ measure (Number 1) teabag

cupOfTea'' :: Recipe
cupOfTea'' = optional
    $ combine "mix" (waitFor (minutes 5)
                        $ combine "pour"
                            (heatTo 100 water) teabag) milk

-- Buttered Toast

butter, bread :: Recipe
butter = ingredient "butter"
bread = ingredient "bread"

toastBread :: Recipe
toastBread = heatAt 600 (ingredient "bread")

toastBreadFor :: Time -> Recipe
toastBreadFor t = addCondition (CondTime t) toastBread

butteredToast :: Recipe
butteredToast = transaction
    $ combine "spread" butter
    $ toastBreadFor (minutes 3)

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

env :: Env
env = Env { eStations = [kettle, chef, toaster]
          , eObs = []
          }

kettle :: Station
kettle = let kettleConstr r@(Node a ts)
                | r == heatTo 100 (ingredient "water") = Just [Input, Output]
                | otherwise = case a of
                    Transaction a -> kettleConstr $ popT r
                    _ -> Nothing
             kettleTemp = return $ ObsTemp 100
          in Station "kettle" [] [] kettleConstr [kettleTemp]

toaster :: Station
toaster = let toasterConstr r@(Node a ts)
                | r == toastBread = Just [Input, Output]
                | otherwise = case a of
                    Conditional _ c@(CondTime t) ->
                        (toasterConstr $ popCond r) >>= return . addEvalCond c
                    Transaction a -> toasterConstr $ popT r
                    _ -> Nothing
              toasterTemp = return $ ObsTemp 600
           in Station "toaster" [] [] toasterConstr [toasterTemp]

chef :: Station
chef = let chefConstr r@(Node a ts) = case a of
                GetIngredient _ -> Just [Input]
                Combine s       -> Just [Input, PCombine s, Output]
                Wait            -> Just [Input, DoNothing, Output]
                Conditional _ c -> (chefConstr $ popCond r)
                                    >>= return . addEvalCond c
                Measure m       -> Just [Input, MeasureOut m, Output]
                Transaction a   -> chefConstr $ popT r
                _               -> Nothing
        in Station "chef" [] [] chefConstr []

-------------------------------------
-- TEST PROPERTIES
-------------------------------------

priceList :: Map Action Price
priceList = Map.fromList $
    map (\(s,i) -> (GetIngredient s, i))
    [ ("teabag", 2)
    , ("milk", 1)
    , ("sugar", 5)
    , ("water", 0)
    ]

priceOfTea :: Price
priceOfTea = lookupProperties priceList cupOfTea

test = let rMap = mkLabelMap $ labelRecipeR butteredToast
           lTree = labelRecipe butteredToast
           lTree' = Recipe.Scheduler.removeFrom lTree 2
        in Recipe.Scheduler.leaves lTree' rMap