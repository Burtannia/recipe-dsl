module Recipe.Printer where

import           Recipe.Recipe
import Data.Tree
import Data.Tree.Pretty

-------------------------------------
-- PRINTING RECIPES
-------------------------------------

toTree :: Recipe -> Tree String
toTree (Ingredient s)    = Node s []
toTree (HeatAt t r)      = Node ("heat at " ++ show t) [toTree r]
toTree (Wait t r)        = Node ("wait for " ++ show t) [toTree r]
toTree (Combine r1 r2)   = Node "combine" [toTree r1, toTree r2]
toTree (Conditional c r) = Node ("condition " ++ show c)  [toTree r]
toTree (Transaction r)   = let Node s xs = toTree r
    in Node (s ++ " (T)") xs
toTree (Measure m r)     = let Node s xs = toTree r
    in Node (s ++ " - " ++ show m) xs

printRecipe :: Recipe -> IO ()
printRecipe = putStrLn . drawVerticalTree . toTree

-------------------------------------
-- PRINTING INGREDIENTS
-------------------------------------

-- Print the list of ingredients in a recipe
printIngredients :: Recipe -> IO ()
printIngredients r = mapM_ putStrLn (getIngredients r)

-------------------------------------
-- PRICE ... this probably should be somewhere else
-------------------------------------

type Price = Float
type PricedItem = (String, Price)
type PriceList = [PricedItem]

class Priced a where
    findCost :: a -> PriceList -> Price

instance Priced Recipe where
    findCost (Ingredient s) ps =
        case prices of
            [] -> 0
            _  -> head prices
        where prices = [p | (x, p) <- ps, x == s]
    findCost (HeatAt _ r) ps      = findCost r ps
    findCost (Combine r1 r2) ps   = findCost r1 ps + findCost r2 ps
    findCost (Wait _ r) ps        = findCost r ps
    findCost (Conditional _ r) ps = findCost r ps
    findCost (Transaction r) ps   = findCost r ps
    findCost (Measure _ r) ps     = findCost r ps

testList :: PriceList
testList = [ ("milk", 1.00)
           , ("teabag", 6.70)
           ]

-- Need to amend this once we have the measurement constructor
-- We don't need a full pack of teabags to make a cup of tea