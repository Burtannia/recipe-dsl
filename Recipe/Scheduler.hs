module Recipe.Scheduler where

import Recipe.Recipe
import Recipe.Kitchen
import Data.LinearProgram
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State
import qualified Data.Map as Map

-- min other values such as stations used?

-- generate end time = start time + duration for all
-- set end time of root recipe as function to min

-- station can't do more than one thing at once
-- each station has a list of recipes with it
-- those can't overlap?

--recipeTime :: Recipe -> LinFunc String Time
