module Recipe.Scheduler where

import Recipe.Recipe
import Data.LinearProgram
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State
import qualified Data.Map as Map

-- label recipes
-- start, duration, end
