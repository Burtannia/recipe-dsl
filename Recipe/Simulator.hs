module Recipe.Simulator where

import           Control.Concurrent

import           Recipe.Printer
import           Recipe.Recipe
import           Recipe.Scheduler

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Wait Time
--             | Sequence Recipe Recipe
--             deriving (Show)

-------------------------------------
-- SIMULATING RECIPES
-------------------------------------

simulateRecipe :: Recipe -> IO ()
simulateRecipe r = putStrLn "Recipe Started\n"
    >> mapM_ simulateRecipe' (extractSteps r)
    >> putStrLn "Recipe Finished"

simulateRecipe' :: LabelledRecipe -> IO ()
simulateRecipe' r = do
    putStrLn $ toStringL r
    putStrLn "Started..."
    threadDelay $ calcStepTime r * 1000000
    putStrLn "Finished..."
    putStrLn ""

-- list of how to prep each labelled item
