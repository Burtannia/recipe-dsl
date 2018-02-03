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

-- Chicken Jalfrezi

-- oliveOil, chicken, cumin, coriander, turmeric :: Recipe
-- redPeppers, garamMasala, tinnedTomatoes, onion :: Recipe
-- garlic, greenChilli, cherryTomatoes :: Recipe
-- chicken = Ingredient "chicken"
-- cumin = Ingredient "cumin"
-- coriander = Ingredient "coriander"
-- turmeric = Ingredient "turmeric"
-- redPeppers = Ingredient "red peppers"
-- garamMasala = Ingredient "garam masala"
-- tinnedTomatoes = Ingredient "tinned tomatoes"
-- onion = Ingredient "onion"
-- garlic = Ingredient "garlic"
-- greenChilli = Ingredient "green chilli"
-- cherryTomatoes = Ingredient "cherry tomatoes"
-- oliveOil = Ingredient "olive oil"

-- spicedChicken :: Recipe
-- spicedChicken = marinate chicken
--     [cumin, coriander, turmeric] 30

-- chickenAndPeppers :: Recipe
-- chickenAndPeppers = heatFor Medium
--     (cookedChicken >< redPeppers) 10
--     where cookedChicken = heatFor Medium
--                             spicedChicken 10

-- jalfreziSauce :: Recipe
-- jalfreziSauce = heatFor Medium
--                 (sauceBase >< spices) 10
--     where
--         onionMix     = onion >< garlic >< greenChilli
--         cookedOnions = heatFor Medium
--                         (preheatOil Medium onionMix) 5
--         spices       = cumin >< coriander
--                         >< turmeric >< garamMasala
--         sauceBase    = cookedOnions >< water
--                         >< tinnedTomatoes >< spices

-- chickenJalfrezi :: Recipe
-- chickenJalfrezi = heatFor Medium chickenJalfrezi' 5
--     where chickenJalfrezi' = chickenAndPeppers
--                              >< jalfreziSauce
--                              >< cherryTomatoes

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