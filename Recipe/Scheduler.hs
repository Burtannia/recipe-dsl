module Recipe.Scheduler where

import           Recipe.Recipe

-- data Recipe = Ingredient String
--             | Heat Temperature Recipe
--             | Combine Recipe Recipe
--             | Wait Time
--             | Sequence Recipe Recipe
--             deriving (Show)

-------------------------------------
-- TIMING RECIPES
-------------------------------------

--Calculates the time a Recipe will take

calcTime :: Recipe -> Int
calcTime (Ingredient _)   = 0
calcTime (Heat t r)       = 0 + calcTime r -- need something here
calcTime (Combine r1 r2)  = calcTime r1 + calcTime r2
calcTime (Wait t)         = t
calcTime (Sequence r1 r2) = calcTime r1 + calcTime r2
-- time to heat something depends on temp, volume and what it is
-- heat capacity and stuff

-- Calculates the time a single LabelledRecipe will take
-- Does not recurse down tree
calcStepTime :: LabelledRecipe -> Int
calcStepTime (LIngredient _)     = 0
calcStepTime (LHeat _ t r)       = 3
calcStepTime (LCombine _ r1 r2)  = case r1 of
    (LWait _ _) -> 0
    _           -> case r2 of
        (LWait _ _) -> 0
        _           -> 5
calcStepTime (LWait _ t)         = t
calcStepTime (LSequence r1 r2)   = 0

atTime :: Int -> Recipe -> Recipe
atTime t r = if t >= calcTime r
                then r
                else case r of
                    (Heat _ r') ->
                        if t >= calcTime r'
                            then r
                            else atTime t r'
                    (Combine r1 r2) -> -- bit awkward because combine gives no promise of order
                        if t >= calcTime r1
                            then atTime t r2
                            else atTime t r1
                    (Sequence r1 r2) ->
                        if t >= calcTime r1
                            then atTime t r2
                            else atTime t r1
                    _ -> r

-------------------------------------
-- SCHEDULING RECIPES
-------------------------------------
