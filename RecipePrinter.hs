module RecipePrinter where

import Recipe

describeRecipe :: Recipe -> String
describeRecipe (Ingredient s) = s
describeRecipe (Heat t r) = "Heat ... to" ++ show t ++ "\n"
describeRecipe (Combine r1 r2) = "Mix " ++ describeRecipe r1 ++ " with " ++ describeRecipe r2 ++ "\n"
describeRecipe (Prepare r) = "Prepare ..." ++ describeRecipe r ++ "\n"
describeRecipe (Wait t) = "Wait" ++ "\n"
describeRecipe (Assemble p r1 r2) = "Take the" ++ describeRecipe r1 ++ show p ++ describeRecipe r2
describeRecipe (After r1 r2) = "Do the following:\n" ++ describeRecipe r1 ++ "Then do the following:\n" ++ describeRecipe r2
describeRecipe (Measure q r) = "Measure " ++ show q ++ "of" ++ describeRecipe r

