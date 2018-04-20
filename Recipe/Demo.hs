module Recipe.Demo where

import           Data.Tree
import           Recipe.Kitchen
import           Recipe.Printer
import           Recipe.Properties
import           Recipe.Recipe
import           Recipe.Scheduler
import           Recipe.Simulator

-------------------------------------
-- TEST RECIPES
-------------------------------------

-- Cup of Tea

water, teabag, milk :: Recipe
water = ingredient "water"
teabag = ingredient "teabag"
milk = ingredient "milk"

cupOfTea :: Recipe
cupOfTea = optional "milk"
    $ combine "mix" milk
    $ waitFor (minutes 5)
    $ combine "mix" teabag
    $ heatTo 100 water

-- cupOfTea == cupOfTea'
cupOfTea' :: Recipe
cupOfTea' = optional "milk"
    $ combine "mix"
    ( waitFor (minutes 5)
    $ combine "mix" teabag
    $ heatTo 100 water ) milk

-- quantified cup of tea
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

-- Chicken Jalfrezi

chicken, redPepper, onion, garlic, tinnedTomatoes, cherryTomatoes :: Recipe
chicken = ingredient "chicken"
redPepper = ingredient "red pepper"
onion = ingredient "onion"
garlic = ingredient "garlic"
tinnedTomatoes = ingredient "tinned tomatoes"
cherryTomatoes = ingredient "cherry tomatoes"

cumin, coriander, turmeric, garamMasala, rice :: Recipe
cumin = ingredient "cumin"
coriander = ingredient "coriander"
turmeric = ingredient "turmeric"
garamMasala = ingredient "garam masala"
rice = ingredient "rice"

spiceMix :: Recipe
spiceMix = multiCombine "mix" cumin [coriander, turmeric]

spicedChicken :: Recipe
spicedChicken = marinate chicken spiceMix (minutes 20)

cookedChicken :: Recipe
cookedChicken = heatForM 10
    $ combine "place in" spicedChicken
    $ preheatOil

jalfreziSauce :: Recipe
jalfreziSauce = combine "mix" tinnedTomatoes
    $ heatForM 10
    $ combine "place in" garlic
    $ combine "place in" onion
    $ preheatOil

chickenJalfrezi :: Recipe
chickenJalfrezi = heatForM 10
    $ multiCombine "mix" cherryTomatoes
        [garamMasala, cookedChicken, jalfreziSauce]

jalfreziWithRice :: Recipe
jalfreziWithRice = combine "on top"
    chickenJalfrezi
    $ boilInWaterForM 10 rice

-------------------------------------
-- CUSTOM COMBINATORS
-------------------------------------

waitFor :: Time -> Recipe -> Recipe
waitFor t r = forTime t (wait r)

multiCombine :: String -> Recipe -> [Recipe] -> Recipe
multiCombine s r [] = r
multiCombine s r rs = foldr (combine s) r rs

marinate :: Recipe -> Recipe -> Time -> Recipe
marinate r m t = waitFor t
    $ heatTo 4
    $ combine "cover in" r m

heatFor :: Time -> Recipe -> Recipe
heatFor t = forTime t . heat

heatForM :: Time -> Recipe -> Recipe
heatForM t = forTime (minutes t) . heat

heatTo :: Int -> Recipe -> Recipe
heatTo t = toTemp t . heat

heatAtFor :: Int -> Time -> Recipe -> Recipe
heatAtFor temp time = forTime time . heatAt temp

heatAtForM :: Int -> Time -> Recipe -> Recipe
heatAtForM temp time = heatAtFor temp (minutes time)

heatAtTo :: Int -> Int -> Recipe -> Recipe
heatAtTo atTmp toTmp = toTemp toTmp . heatAt atTmp

oliveOil :: Recipe
oliveOil = ingredient "olive oil"

preheatOil :: Recipe
preheatOil = heatForM 2 oliveOil

boilInWaterForM :: Time -> Recipe -> Recipe
boilInWaterForM t r = forTime (minutes t)
    $ combine "place in" r
    $ heatTo 100 water

-------------------------------------
-- TEST STATIONS
-------------------------------------

env :: Env
env = Env { eStations = [kettle, chef, toaster, hob, chef2]
          , eObs = [ return $ ObsTime 0
                   , return $ ObsOpt "milk" True ] }

isBoilWater :: Recipe -> Bool
isBoilWater r@(Node a ts) = case a of
    Conditional Heat (CondTemp 100) -> case ts of
        [Node (GetIngredient "water") []]                    -> True
        [Node (Measure _) [Node (GetIngredient "water") []]] -> True
        _                                                    -> False
    Transaction _ -> isBoilWater $ popT r
    _ -> False

kettle :: Station
kettle = let kettleConstr r =
                    if isBoilWater r then
                        Just [Input, Output]
                    else
                        Nothing
             kettleTemp = return $ ObsTemp 10
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
                GetIngredient _ -> Just [Input, Output]
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
                GetIngredient _ -> Just [Input, Output]
                Combine s       -> Just [Input, PCombine s, Output]
                Wait            -> Just [Input, DoNothing, Output]
                Conditional _ c -> (chefConstr $ popCond r)
                                    >>= return . addEvalCond c
                Measure m       -> Just [Input, MeasureOut m, Output]
                Transaction a   -> chefConstr $ popT r
                _               -> Nothing
         in Station "chef2" chefConstr []

hob :: Station
hob =
    let hobConstr r@(Node a ts) = case a of
            Heat -> Just [Input, Output]
            Wait -> Just [Input, DoNothing, Output]
            Conditional _ c -> (hobConstr $ popCond r)
                                >>= return . addEvalCond c
            Transaction a -> hobConstr $ popT r
            _ -> Nothing
     in Station "hob" hobConstr []

hob2 :: Station
hob2 =
    let hobConstr r@(Node a ts) = case a of
            Heat -> Just [Input, Output]
            Wait -> Just [Input, DoNothing, Output]
            Conditional _ c -> (hobConstr $ popCond r)
                                >>= return . addEvalCond c
            Transaction a -> hobConstr $ popT r
            _ -> Nothing
     in Station "hob2" hobConstr []

fridge :: Station
fridge =
    let fridgeConstr r@(Node a ts) = case a of
            Conditional Heat (CondTemp 4) -> Just [Input, Output]
            Transaction a                 -> fridgeConstr $ popT r
            _                             -> Nothing
     in Station "fridge" fridgeConstr []

-------------------------------------
-- TEST PROPERTIES
-------------------------------------

-- Price

priceList :: PriceList
priceList = [ ("teabag", (639, Count 240))
            , ("milk", (70, Milliletres 1000))
            , ("sugar", (69, Grams 1000))
            , ("water", (0, Milliletres 1)) ]

-- Food Type

data FoodType = Meat | Veg
    deriving (Show, Eq)

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

-- Cuisine

meatTwoVeg :: Cuisine (FoodType, Recipe)
meatTwoVeg ps =
    let meats = filter (\(_,p) -> fst p == Meat) ps
        vegs = filter (\(_,p) -> fst p == Veg) ps
        parts = map (\(_,p) -> snd p)
                $ head meats : take 2 vegs
     in foldr1 (combine "next to") parts

testRecipe :: Recipe
testRecipe = meatTwoVeg pList
    where
        pList = map (\((s, t), (_, r)) -> (s, (t, r))) lists
        lists = zip ingList recList

-- Seasoning

guacamole :: Recipe
guacamole = multiCombine "mix" avocado
    [salt, limeJuice]

avocado, salt, limeJuice :: Recipe
avocado = ingredient "avocado"
salt = ingredient "salt"
limeJuice = ingredient "lime juice"

seasonings :: PropertyList Seasoning
seasonings = [ ("avocado", Sweet)
             , ("salt", Salt)
             , ("lime juice", Acid) ]
