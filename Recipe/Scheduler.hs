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

calcTime :: Recipe -> Int
calcTime (Ingredient _)   = 0
calcTime (Heat t r)       = 0 + calcTime r -- need something here
calcTime (Combine r1 r2)  = calcTime r1 + calcTime r2
calcTime (Wait t)         = t
calcTime (Sequence r1 r2) = calcTime r1 + calcTime r2
-- time to heat something depends on temp, volume and what it is

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

type Colour = Int
type ColouredRecipe = (Colour, Recipe)

type RecipeSchedule = [ColouredRecipe]

-- Given a number of paths of execution (each
-- performing steps one at a time), assigns
-- steps between the paths
-- scheduleLinear :: Recipe -> RecipeSchedule
-- scheduleLinear r 1 = [(1, r)]

--scheduleLinear' :: [Recipe] -> RecipeSchedule
-- implement graph colouring

-- Given a number of paths of execution (each
-- performing multiple steps concurrently),
-- assigns steps between the paths
-- scheduleConcurrent :: Recipe -> Int -> RecipeSchedule
