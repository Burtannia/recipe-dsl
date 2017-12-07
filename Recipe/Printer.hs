module Recipe.Printer where

import           Recipe.Recipe

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

-------------------------------------
-- PRINTING INGREDIENTS
-------------------------------------

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

-- Need to amend this once we have the measurement constructor
-- We don't need a full pack of teabags to make a cup of tea