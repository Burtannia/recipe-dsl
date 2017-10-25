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

showLabel :: LabelledRecipe -> String
-- showLabel r = case r of
--               (LIngredient s) -> s
--               _ -> (show . getLabel) r
showLabel (LIngredient s)   = s
showLabel (LSequence r1 r2) = show $ if l1 > l2 then l1 else l2
                              where
                                l1 = getLabel r1
                                l2 = getLabel r2
showLabel r                 = (show . getLabel) r

toStringL :: LabelledRecipe -> String
toStringL (LIngredient s)    = s
toStringL (LHeat l t r)      = show l ++ ") Heat (" ++ showLabel r ++ ") to" ++ show t
toStringL (LCombine l r1 r2) = show l ++ ") Mix (" ++ showLabel r1 ++ ") with (" ++ showLabel r2 ++ ")"
toStringL (LWait l t)        = show l ++ ") Wait for " ++ show t
toStringL (LSequence _ _)    = "" -- this won't exist after extractSteps has been called

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
printSteps r = mapM_ (putStrLn . toStringL) (extractSteps r)

-- Great but should label steps
-- Don't print out ingredients at the start
-- Keep ingredient names for Heat etc. but use step label if not ingredient

-- Parallel steps

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

-- Obviously this is rubbish... We need to take into account amortised costs
-- type PricedItem = (String, Price, Quantity)
-- Use the measurement constructor on recipes, at the moment we have a cooking show
-- setup where we presume everything is already chopped and measured etc.
