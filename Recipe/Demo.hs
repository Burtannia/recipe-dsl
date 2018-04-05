module Recipe.Demo where

import Recipe.Recipe
import Recipe.Printer
import Recipe.Kitchen
import Recipe.Scheduler
import Data.Tree
import Recipe.Properties
import Recipe.QS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Time.Clock
import Data.Monoid

-------------------------------------
-- TEST RECIPES
-------------------------------------

-- Cup of Tea

water, teabag, milk :: Recipe
water = ingredient "water"
teabag = ingredient "teabag"
milk = ingredient "milk"

cupOfTea :: Recipe
cupOfTea = optional "milk" $ combine "mix" milk
    $ waitFor (minutes 5)
    $ combine "mix" teabag
    $ heatTo 100 water

cupOfTea' :: Recipe
cupOfTea' = optional "milk" $ combine "mix" 
    ( waitFor (minutes 5)
    $ combine "mix" teabag
    $ heatTo 100 water ) milk

cupOfTeaQ :: Recipe
cupOfTeaQ = optional "milk"
    $ combine "mix" (measure (Milliletres 10) milk)
    $ waitFor (minutes 5)
    $ combine "mix" (measure (Count 1) teabag)
    $ heatTo 100
    $ measure (Milliletres 300) water

-- Buttered Toast

butter, bread :: Recipe
butter = ingredient "butter"
bread = ingredient "bread"

butteredToast :: Recipe
butteredToast = transaction
    $ combine "spread" butter
    $ heatFor (minutes 3) bread

teaWithToast :: Recipe
teaWithToast = combine "place next to"
    butteredToast
    cupOfTea

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

heatAtForM :: Int -> Time -> Recipe -> Recipe
heatAtForM temp (Time i) = heatAtFor temp (minutes i)

heatAtTo :: Int -> Int -> Recipe -> Recipe
heatAtTo atTmp toTmp = toTemp toTmp . heatAt atTmp

oliveOil :: Recipe
oliveOil = ingredient "olive oil"

preheatOil :: Int -> Int -> Recipe
preheatOil = \atTmp toTmp ->
    heatAtTo atTmp toTmp oliveOil

boilInWaterForM :: Time -> Recipe -> Recipe
boilInWaterForM t r = forTime (t * 60) 
    $ combine "place in" r
    $ heatTo 100 water

-------------------------------------
-- TEST STATIONS
-------------------------------------

test :: IO ()
test = do
    c <- newClock
    getTime' c >>= print
    tick c
    getTime' c >>= print
    tick c
    getTime' c >>= print

tick :: Clock -> IO ()
tick c = modifyIORef' c (+1)

newClock :: IO Clock
newClock = newIORef 0

type Clock = IORef Int

getTime' :: Clock -> IO Obs
getTime' c = do
    i <- readIORef c
    (return . ObsTime . Time) i

getTime :: IO Obs
getTime = do
    ut <- getCurrentTime
    let t = floor $ utctDayTime ut
    let obs = (ObsTime . Time) t
    return obs

env :: Env
env = Env { eStations = [kettle, chef, toaster]
          , eObs = [ getTime
                   , return $ ObsOpt "milk" True ] }

isBoilWater :: Recipe -> Bool
isBoilWater r@(Node a ts) = case a of
    Conditional Heat (CondTemp 100) -> case ts of
        [Node (GetIngredient "water") []] -> True
        [Node (Measure _) [Node (GetIngredient "water") []]] -> True
        _ -> False
    Transaction _ -> isBoilWater $ popT r
    _ -> False

kettle :: Station
kettle = let kettleConstr r =
                    if isBoilWater r then
                        Just [Input, Output]
                    else
                        Nothing
             kettleTemp = return $ ObsTemp 100
          in Station "kettle" kettleConstr [kettleTemp]

toastBread :: Recipe
toastBread = heat (ingredient "bread")

toaster :: Station
toaster = let toasterConstr r@(Node a ts)
                | r == toastBread = Just [Input, Output]
                | otherwise = case a of
                    Conditional _ c@(CondTime t) ->
                        (toasterConstr $ popCond r) >>= return . addEvalCond c
                    Transaction a -> toasterConstr $ popT r
                    _ -> Nothing
              toasterTemp = return $ ObsTemp 600
           in Station "toaster" toasterConstr [toasterTemp]

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
        in Station "chef" chefConstr []

chef2 :: Station
chef2 = let chefConstr r@(Node a ts) = case a of
                GetIngredient _ -> Just [Input]
                Combine s       -> Just [Input, PCombine s, Output]
                Wait            -> Just [Input, DoNothing, Output]
                Conditional _ c -> (chefConstr $ popCond r)
                                    >>= return . addEvalCond c
                Measure m       -> Just [Input, MeasureOut m, Output]
                Transaction a   -> chefConstr $ popT r
                _               -> Nothing
         in Station "chef2" chefConstr []

-------------------------------------
-- TEST PROPERTIES
-------------------------------------

priceList :: PriceList
priceList = [ ("teabag", (639, Count 240))
            , ("milk", (70, Milliletres 1000))
            , ("sugar", (69, Grams 1000))
            , ("water", (0, Milliletres 1)) ]

ingList :: PropertyList FoodType
ingList = [ ("chicken breast", Meat)
          , ("beef sirloin", Meat)
          , ("lamb chop", Meat)
          , ("broccoli", Veg)
          , ("carrot", Veg)
          , ("cauliflower", Veg)
          , ("green beans", Veg)
          , ("green cabbage", Veg)
          , ("potato", Veg) ]

recList :: PropertyList Recipe
recList = [ ("chicken breast", heatAtForM 200 40 $ ingredient "chicken breast")
          , ("beef sirloin", heatAtForM 200 20 $ ingredient "beef sirloin")
          , ("lamb chop", heatAtForM 200 17 $ ingredient "lamb chop")
          , ("broccoli", boilInWaterForM 5 $ ingredient "broccoli")
          , ("carrot", boilInWaterForM 5 $ ingredient "carrot")
          , ("cauliflower", boilInWaterForM 7 $ ingredient "cauliflower")
          , ("green beans", boilInWaterForM 5 $ ingredient "green beans")
          , ("green cabbage", boilInWaterForM 5 $ ingredient "green cabbage") ]

testRecipe :: Recipe
testRecipe = mkRecipe (meatTwoVeg ingList) recList