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


