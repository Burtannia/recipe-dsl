module Recipe.Printer where

import           Recipe

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Wait Time
--             | Sequence Recipe Recipe
--             deriving (Show)

-------------------------------------
-- PRINTING RECIPES
-------------------------------------

toString :: Recipe -> String
toString (Ingredient s)   = s
toString (Heat t r)       = "Heat (" ++ toString r ++ ") to " ++ show t
toString (Combine r1 r2)  = "Mix (" ++ toString r1 ++ ") with (" ++ toString r2 ++ ")"
toString (Wait t)         = "Wait for " ++ show t
toString (Sequence r1 r2) = "Do (" ++ toString r1 ++ ") then (" ++ toString r2 ++ ")"

printRecipe :: Recipe -> IO ()
printRecipe x@(Ingredient _)   = putStrLn $ toString x
printRecipe x@(Heat _ r)       = printRecipe r
                                    >> putStrLn (toString x)
printRecipe x@(Combine r1 r2)  = printRecipe r1
                                    >> printRecipe r2
                                    >> putStrLn (toString x)
printRecipe x@(Wait _)         = putStrLn $ toString x
printRecipe x@(Sequence r1 r2) = printRecipe r1
                                    >> putStrLn (toString x)
                                    >> printRecipe r2

printSteps :: Recipe -> IO ()
printSteps (Ingredient _)    = return ()
printSteps x@(Heat _ r)      = printSteps r >> (putStrLn . toString) x
printSteps x@(Combine r1 r2) = printSteps r1 >> printSteps r2 >> (putStrLn . toString) x
printSteps (Sequence r1 r2)  = printSteps r1 >> printSteps r2
printSteps r                 = putStrLn $ toString r

-- Great but should label steps
-- Don't print out ingredients at the start
-- Keep ingredient names for Heat etc. but use step label if not ingredient

-- Parallel steps

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
-- Use the measurement constructor on recipes, at the moment we have a cooking show
-- setup where we presume everything is already chopped and measured etc.
