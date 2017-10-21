module RecipePrinter where

import           Recipe

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Wait Time
--             | Sequence Recipe Recipe
--             deriving (Show)

-- Recipe -> Int (change in time)

-------------------------------------
-- PRINTING RECIPES
-------------------------------------

toString :: Recipe -> String
toString (Ingredient s) = s
toString (Heat t _)     = "Heat to " ++ show t
toString (Combine _ _)  = "Mix"
toString (Wait t)       = "Wait for " ++ show t
toString (Sequence _ _) = "Then"

-------------------------------------
-- PRINTING INGREDIENTS
-------------------------------------

-- Create a list of ingredients in a recipe
getIngredients :: Recipe -> [String]
getIngredients (Ingredient s)   = [s]
getIngredients (Heat _ r)       = getIngredients r
getIngredients (Combine r1 r2)  = getIngredients r1 ++ getIngredients r2
getIngredients (Wait _)         = []
getIngredients (Sequence r1 r2) = getIngredients r1 ++ getIngredients r2

-- Print the list of ingredients in a recipe
printIngredients :: Recipe -> IO ()
printIngredients r = mapM_ putStrLn (getIngredients r)

-------------------------------------
-- PRICE
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
    findCost (Heat _ r) ps       = findCost r ps
    findCost (Combine r1 r2) ps  = findCost r1 ps + findCost r2 ps
    findCost (Wait _) _          = 0
    findCost (Sequence r1 r2) ps = findCost r1 ps + findCost r2 ps

testList :: PriceList
testList = [ ("milk", 1.00)
           , ("teabag", 6.70)
           ]

-- Obviously this is rubbish... We need to take into account amortised costs
-- type PricedItem = (String, Price, Quantity)
-- Use the measurement constructor on recipes
