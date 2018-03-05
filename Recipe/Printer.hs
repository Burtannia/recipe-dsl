module Recipe.Printer where

import           Recipe.Recipe
import Data.Char
import Data.List
import Data.Tree
import Data.Tree.Pretty
import Control.Monad.State

-------------------------------------
-- PRINTING RECIPES
-------------------------------------

stringTree :: Recipe -> Tree String
stringTree = recipeToTree toString

toString :: Recipe -> String
toString r = case r of
    Ingredient s    -> s
    HeatAt t r      -> "heat at " ++ show t
    Wait t r        -> "wait for " ++ show t
    Combine r1 r2   -> "combine"
    Conditional c r -> "condition " ++ show c
    Transaction r   -> "transaction"
    Measure m r     -> "measure : " ++ show m

printRecipe :: Recipe -> IO ()
printRecipe = putStrLn . drawVerticalTree . stringTree

-------------------------------------
-- PRINTING INGREDIENTS
-------------------------------------

-- Print the list of ingredients in a recipe
printIngredients :: Recipe -> IO ()
printIngredients r = mapM_ putStrLn (ingredients r)

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