module Recipe.Demo where

import Recipe.Recipe
import Recipe.Printer
import Recipe.Kitchen
import Data.Tree
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

env :: Env
env = Env { eStations = [kettle]
          , eObs = []
          }

kettle :: Station
kettle = let kettleConstr r 
                | r == heatTo 100 (ingredient "water") = Just [Input, Output]
                | otherwise = Nothing
             kettleTemp = return $ ObsTemp 100 in
         Station "kettle" [] [] kettleConstr [kettleTemp]

chef :: Station
chef = let chefConstr r@(Node a ts) = case a of
                GetIngredient _ -> Just [Input]
                Combine s -> Just [Input, PCombine s, Output]
                Wait -> Just [Input, DoNothing, Output]
                Conditional a c -> (chefConstr $ popCond r)
                    >>= return . addEvalCond c
                _ -> Nothing in
       Station "chef" [] [] chefConstr []

popCond :: Recipe -> Recipe
popCond (Node (Conditional a c) ts) = Node a ts
popCond r = r

addEvalCond :: Condition -> [Process] -> [Process]
addEvalCond c (Input:ps) = Input : EvalCond c : ps
addEvalCond c ps = EvalCond c : ps